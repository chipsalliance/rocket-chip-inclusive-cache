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

class SinkDResponse(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val last   = Bool()
  val opcode = UInt(width = 3)
  val param  = UInt(width = 3)
  val source = UInt(width = params.outer.bundle.sourceBits)
  val sink   = UInt(width = params.outer.bundle.sinkBits)
  val denied = Bool()
}

class SinkD(params: InclusiveCacheParameters) extends Module
{
  val io = new Bundle {
    val resp = Valid(new SinkDResponse(params)) // Grant or ReleaseAck
    val d = Decoupled(new TLBundleD(params.outer.bundle)).flip
    /** request to MSHR with tilelink sourceId:
      * ask for set(D channel don't have address) and way
      */
    val source = UInt(width = params.outer.bundle.sourceBits)
    /** way response from MSHR. */
    val way    = UInt(width = params.wayBits).flip
    /** set response from MSHR. */
    val set    = UInt(width = params.setBits).flip
    /** address access to banked store only. */
    val bs_adr = Decoupled(new BankedStoreOuterAddress(params))
    /** data write to banked store. */
    val bs_dat = new BankedStoreOuterPoison(params)
    /* WaR hazard.
     * @todo
     */
    val grant_req = new SourceDHazard(params)
    val grant_safe = Bool().flip
  }

  // No restrictions on buffer
  val d = params.micro.outerBuf.d(io.d)

  val (first, last, _, beat) = params.outer.count(d)
  val hasData = params.outer.hasData(d.bits)

  /* latch [[d.bits.source]] with [[d.valid]]. */
  io.source := Mux(d.valid, d.bits.source, RegEnable(d.bits.source, d.valid))
  /* forward MSHR way, set reply to SourceD directly. */
  io.grant_req.way := io.way
  io.grant_req.set := io.set

  /* Also send Grant(NoData) to BS to ensure correct data ordering.
   * used for [[MSHR.w_grantfirst]] and [[MSHR.w_grantlast]]
   */
  io.resp.valid := (first || last) && d.fire()
  /* d channel ready:
   * banked store ready to access.
   * sourceD tell us this grant is safe.(only first beat need to check)
   */
  d.ready := io.bs_adr.ready && (!first || io.grant_safe)
  /* first beat access to banked store need [[io.grant_safe]].
   * after receive first beat, block others access to the reset of this cacheline with [[io.bs_adr.bits.noop]]
   */
  io.bs_adr.valid := !first || (d.valid && io.grant_safe)
  params.ccover(d.valid && first && !io.grant_safe, "SINKD_HAZARD", "Prevented Grant data hazard with backpressure")
  params.ccover(io.bs_adr.valid && !io.bs_adr.ready, "SINKD_SRAM_STALL", "Data SRAM busy")

  io.resp.bits.last   := last
  io.resp.bits.opcode := d.bits.opcode
  io.resp.bits.param  := d.bits.param
  io.resp.bits.source := d.bits.source
  io.resp.bits.sink   := d.bits.sink
  io.resp.bits.denied := d.bits.denied

  /* block others with noop */
  io.bs_adr.bits.noop := !d.valid || !hasData
  io.bs_adr.bits.way  := io.way
  io.bs_adr.bits.set  := io.set
  io.bs_adr.bits.beat := Mux(d.valid, beat, RegEnable(beat + io.bs_adr.ready.asUInt, d.valid))
  io.bs_adr.bits.mask := ~UInt(0, width = params.outerMaskBits)
  io.bs_dat.data      := d.bits.data

  assert (!(d.valid && d.bits.corrupt && !d.bits.denied), "Data poisoning unsupported")
}
