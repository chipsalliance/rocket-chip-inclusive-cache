/*
 * Copyright 2019 SiFive, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You should have received a copy of LICENSE.Apache2 along with
 * this software. If not, you may obtain a copy at
 *
 *    https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sifive.blocks.inclusivecache

import Chisel._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class SinkCResponse(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val last   = Bool()
  val set    = UInt(width = params.setBits)
  val tag    = UInt(width = params.tagBits)
  val source = UInt(width = params.inner.bundle.sourceBits)
  val param  = UInt(width = 3)
  val data   = Bool()
}

class PutBufferCEntry(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val data = UInt(width = params.inner.bundle.dataBits)
  val corrupt = Bool()
}

class SinkC(params: InclusiveCacheParameters) extends Module
{
  val io = new Bundle {
    /** to MSHR notify allocate release request. */
    val req = Decoupled(new FullRequest(params)) // Release
    /** from MSHR ProbeAck is received. */
    val resp = Valid(new SinkCResponse(params)) // ProbeAck
    /** TileLink C channel from client. */
    val c = Decoupled(new TLBundleC(params.inner.bundle)).flip
    /** signal to MSHR, give set, ask for way to access banked store. */
    val set = UInt(width = params.setBits)
    /** MSHR CAM lookup will return a way to us. */
    val way = UInt(width = params.wayBits).flip
    /** ProbeAckData from other clients to banked store. */
    val bs_adr = Decoupled(new BankedStoreInnerAddress(params))
    /** ProbeAckData data. */
    val bs_dat = new BankedStoreInnerPoison(params)
    /** SourceD ask this to pop a entry from putbuffer. */
    val rel_pop  = Decoupled(new PutBufferPop(params)).flip
    /** ReleaseData from this to [[putbuffer]] data to SourceD. */
    val rel_beat = new PutBufferCEntry(params)
  }

  if (params.firstLevel) {
    // Tie off unused ports
    io.req.valid := Bool(false)
    io.resp.valid := Bool(false)
    io.c.ready := Bool(true)
    io.set := UInt(0)
    io.bs_adr.valid := Bool(false)
    io.rel_pop.ready := Bool(true)
  } else {
    // No restrictions on the type of buffer
    val c = params.micro.innerBuf.c(io.c)

    val (tag, set, offset) = params.parseAddress(c.bits.address)
    val (first, last, _, beat) = params.inner.count(c)
    /** transaction has data. */
    val hasData = params.inner.hasData(c.bits)
    /** transaction in C channel a ProbeAck? */
    val raw_resp = c.bits.opcode === TLMessages.ProbeAck || c.bits.opcode === TLMessages.ProbeAckData
    /** latch raw_resp. */
    val resp = Mux(c.valid, raw_resp, RegEnable(raw_resp, c.valid))

    // Handling of C is broken into two cases:
    //   ProbeAck
    //     if hasData, must be written to BankedStore
    //     if last beat, trigger resp
    //   Release
    //     if first beat, trigger req
    //     if hasData, go to putBuffer
    //     if hasData && first beat, must claim a list

    assert (!(c.valid && c.bits.corrupt), "Data poisoning unavailable")

    /** latch: [[set]]. */
    io.set := Mux(c.valid, set, RegEnable(set, c.valid)) // finds us the way

    /* Cut path from inner C to the BankedStore SRAM setup
     * this makes it easier to layout the L2 data banks far away
     * @todo timming here?
     */
    val bs_adr = Wire(io.bs_adr)
    io.bs_adr <> Queue(bs_adr, 1, pipe=true)
    /* latch data in a clock-gated register, in case of ProbeAck cause useless flip. C */
    io.bs_dat.data   := RegEnable(c.bits.data,    bs_adr.fire())
    /* even if a truncated burst comes, valid will be hold to high. C */
    bs_adr.valid     := resp && (!first || (c.valid && hasData))
    /* block other request if this is burst data not finish. C */
    bs_adr.bits.noop := !c.valid
    bs_adr.bits.way  := io.way
    bs_adr.bits.set  := io.set
    /* if [[c.valid]] access banked store with beat as address.
     * if [[!c.valid]] block banked store with last `beat + 1.U`.
     */
    bs_adr.bits.beat := Mux(c.valid, beat, RegEnable(beat + bs_adr.ready.asUInt, c.valid))
    bs_adr.bits.mask := ~UInt(0, width = params.innerMaskBits)
    params.ccover(bs_adr.valid && !bs_adr.ready, "SINKC_SRAM_STALL", "Data SRAM busy")

    /* ProbeAck: notify MSHR directly.
     * ProbeAckData: notify MSHR when BankedStore accepted data.
     */
    io.resp.valid := resp && c.valid && (first || last) && (!hasData || bs_adr.ready)
    io.resp.bits.last   := last
    io.resp.bits.set    := set
    io.resp.bits.tag    := tag
    io.resp.bits.source := c.bits.source
    io.resp.bits.param  := c.bits.param
    io.resp.bits.data   := hasData

    /* put buffer for Release/ReleaseData.
     * same as SInkA.
     */
    val putbuffer = Module(new ListBuffer(ListBufferParameters(new PutBufferCEntry(params), params.relLists, params.relBeats, false)))
    val lists = RegInit(UInt(0, width = params.relLists))

    val lists_set = Wire(init = UInt(0, width = params.relLists))
    val lists_clr = Wire(init = UInt(0, width = params.relLists))
    lists := (lists | lists_set) & ~lists_clr

    val free = !lists.andR()
    val freeOH = ~(leftOR(~lists) << 1) & ~lists
    val freeIdx = OHToUInt(freeOH)

    val req_block = first && !io.req.ready
    val buf_block = hasData && !putbuffer.io.push.ready
    val set_block = hasData && first && !free

    params.ccover(c.valid && !raw_resp && req_block, "SINKC_REQ_STALL", "No MSHR available to sink request")
    params.ccover(c.valid && !raw_resp && buf_block, "SINKC_BUF_STALL", "No space in putbuffer for beat")
    params.ccover(c.valid && !raw_resp && set_block, "SINKC_SET_STALL", "No space in putbuffer for request")

    /* mux based on if this transaction is ProbeAck(Data) or ReleaseAck(data). */
    c.ready := Mux(raw_resp, !hasData || bs_adr.ready, !req_block && !buf_block && !set_block)

    io.req.valid := !resp && c.valid && first && !buf_block && !set_block
    putbuffer.io.push.valid := !resp && c.valid && hasData && !req_block && !set_block
    when (!resp && c.valid && first && hasData && !req_block && !buf_block) { lists_set := freeOH }

    val put = Mux(first, freeIdx, RegEnable(freeIdx, first))

    /* spawn a request to scheduler, allocate a MSHR, modify directory. */
    io.req.bits.prio   := Vec(UInt(4, width=3).asBools)
    io.req.bits.control:= Bool(false)
    io.req.bits.opcode := c.bits.opcode
    io.req.bits.param  := c.bits.param
    io.req.bits.size   := c.bits.size
    io.req.bits.source := c.bits.source
    io.req.bits.offset := offset
    io.req.bits.set    := set
    io.req.bits.tag    := tag
    io.req.bits.put    := put

    putbuffer.io.push.bits.index := put
    putbuffer.io.push.bits.data.data    := c.bits.data
    putbuffer.io.push.bits.data.corrupt := c.bits.corrupt

    // Grant access to pop the data
    putbuffer.io.pop.bits := io.rel_pop.bits.index
    putbuffer.io.pop.valid := io.rel_pop.fire()
    io.rel_pop.ready := putbuffer.io.valid(io.rel_pop.bits.index)
    io.rel_beat := putbuffer.io.data

    when (io.rel_pop.fire() && io.rel_pop.bits.last) {
      lists_clr := UIntToOH(io.rel_pop.bits.index, params.relLists)
    }
  }
}
