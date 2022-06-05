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

class SourceCRequest(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val opcode = UInt(width = 3)
  val param  = UInt(width = 3)
  val source = UInt(width = params.outer.bundle.sourceBits)
  val tag    = UInt(width = params.tagBits)
  val set    = UInt(width = params.setBits)
  val way    = UInt(width = params.wayBits)
  val dirty  = Bool()
}

class SourceC(params: InclusiveCacheParameters) extends Module
{
  val io = new Bundle {
    /** request from scheduler. */
    val req = Decoupled(new SourceCRequest(params)).flip
    val c = Decoupled(new TLBundleC(params.outer.bundle))
    // BankedStore port
    val bs_adr = Decoupled(new BankedStoreOuterAddress(params))
    val bs_dat = new BankedStoreOuterDecoded(params).flip
    // RaW hazard
    val evict_req = new SourceDHazard(params)
    val evict_safe = Bool().flip
  }

  // We ignore the depth and pipe is useless here (we have to provision for worst-case=stall)
  require (!params.micro.outerBuf.c.pipe)

  val beatBytes = params.outer.manager.beatBytes
  /* burst beats. */
  val beats = params.cache.blockBytes / beatBytes
  val flow = params.micro.outerBuf.c.flow
  /** */
  val queue = Module(new Queue(io.c.bits, beats + 3 + (if (flow) 0 else 1), flow = flow))

  /** [[queue.io.count]] is far too slow.
    * maintain a local counter here.
    */
  val fillBits = log2Up(beats + 4)
  /** equal to [[queue.io.count]]. */
  val fill = RegInit(UInt(0, width = fillBits))
  /** queue has enough room to store next burst.
    * @todo why room here?
    */
  val room = RegInit(Bool(true))
  /* update [[fill]] and [[room]]. */
  when (queue.io.enq.fire() =/= queue.io.deq.fire()) {
    fill := fill + Mux(queue.io.enq.fire(), UInt(1), ~UInt(0, width = fillBits))
    room := fill === UInt(0) || ((fill === UInt(1) || fill === UInt(2)) && !queue.io.enq.fire())
  }
  assert (room === queue.io.count <= UInt(1))

  /** processing a [[ReleaseData]] burst. */
  val busy = RegInit(Bool(false))
  /** the current beat of burst. */
  val beat = RegInit(UInt(0, width = params.outerBeatBits))
  /** since beatbyte is exp of 2, beat.andR is the last beat. */
  val last = beat.andR
  /** latch request. */
  val req  = Mux(!busy, io.req.bits, RegEnable(io.req.bits, !busy && io.req.valid))
  /** need to access bankedstore. */
  val want_data = busy || (io.req.valid && room && io.req.bits.dirty)

  io.req.ready := !busy && room

  io.evict_req.set := req.set
  io.evict_req.way := req.way

  /* only first beat need to check [[io.evict_safe]] */
  io.bs_adr.valid := (beat.orR || io.evict_safe) && want_data
  io.bs_adr.bits.noop := Bool(false)
  io.bs_adr.bits.way  := req.way
  io.bs_adr.bits.set  := req.set
  io.bs_adr.bits.beat := beat
  io.bs_adr.bits.mask := ~UInt(0, width = params.outerMaskBits)

  params.ccover(io.req.valid && io.req.bits.dirty && room && !io.evict_safe, "SOURCEC_HAZARD", "Prevented Eviction data hazard with backpressure")
  params.ccover(io.bs_adr.valid && !io.bs_adr.ready, "SOURCEC_SRAM_STALL", "Data SRAM busy")

  /* update [[busy]] and [[beat]]. */
  when (io.req.valid && room && io.req.bits.dirty) { busy := Bool(true) }
  when (io.bs_adr.fire()) {
    when (last) { busy := Bool(false) }
    beat := beat + UInt(1)
  }

  /* banked store access takes 2 cycles. */
  /** if [[want_data]], need to access bankedstore, else just need [[io.req]]*/
  val s2_latch = Mux(want_data, io.bs_adr.fire(), io.req.fire())
  val s2_valid = RegNext(s2_latch)
  val s2_req = RegEnable(req, s2_latch)
  val s2_beat = RegEnable(beat, s2_latch)
  val s2_last = RegEnable(last, s2_latch)

  val s3_latch = s2_valid
  val s3_valid = RegNext(s3_latch)
  val s3_req = RegEnable(s2_req, s3_latch)
  val s3_beat = RegEnable(s2_beat, s3_latch)
  val s3_last = RegEnable(s2_last, s3_latch)

  val c = Wire(io.c)
  c.valid        := s3_valid
  c.bits.opcode  := s3_req.opcode
  c.bits.param   := s3_req.param
  c.bits.size    := UInt(params.offsetBits)
  c.bits.source  := s3_req.source
  c.bits.address := params.expandAddress(s3_req.tag, s3_req.set, UInt(0))
  c.bits.data    := io.bs_dat.data
  c.bits.corrupt := Bool(false)

  // We never accept at the front-end unless we're sure things will fit
  assert(!c.valid || c.ready)
  params.ccover(!c.ready, "SOURCEC_QUEUE_FULL", "Eviction queue fully utilized")

  queue.io.enq <> c
  io.c <> queue.io.deq
}
