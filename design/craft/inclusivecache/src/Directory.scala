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
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import MetaData._
import freechips.rocketchip.util.DescribedSRAM

class DirectoryEntry(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  /** true => TRUNK or TIP. */
  val dirty   = Bool()
  /** TileLink status Tip/Trunk/Branch/Invalid. */
  val state   = UInt(width = params.stateBits)
  /** indicate which masters(Tip/Branch/Trunk) has this cacheline. */
  val clients = UInt(width = params.clientBits)
  /** check if a address is in current set. */
  val tag     = UInt(width = params.tagBits)
}

class DirectoryWrite(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val set  = UInt(width = params.setBits)
  val way  = UInt(width = params.wayBits)
  val data = new DirectoryEntry(params)
}

class DirectoryRead(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val set = UInt(width = params.setBits)
  val tag = UInt(width = params.tagBits)
}

/** reply to [[DirectoryRead]]. */
class DirectoryResult(params: InclusiveCacheParameters) extends DirectoryEntry(params)
{
  val hit = Bool()
  val way = UInt(width = params.wayBits)
}

/** A bank has a directory to record metadata.
  * 1. read not cached.
  *
  * {{{
  *   W    R
  *
  *    SRAM
  * }}}
  *   0 1 2
  * W ? w d   // w-> write, d -> done
  * R
  *
  *
  * 2. read cached.
  *
  */
class Directory(params: InclusiveCacheParameters) extends Module
{
  val io = new Bundle {
    val write  = Decoupled(new DirectoryWrite(params)).flip
    /** sees same-cycle write. */
    val read   = Valid(new DirectoryRead(params)).flip
    val result = Valid(new DirectoryResult(params))
    /** Directory reset complete, can enable access. */
    val ready  = Bool()
  }

  /** width of SRAM. */
  val codeBits = new DirectoryEntry(params).getWidth

  val (cc_dir, omSRAM) =  DescribedSRAM(
    name = "cc_dir",
    desc = "Directory RAM",
    size = params.cache.sets,
    data = Vec(params.cache.ways, UInt(width = codeBits))
  )

  /* must inspect contents => max size 1
   * a flow Q creates a WaR hazard... this MIGHT not cause a problem
   * a pipe Q causes combinational loop through the scheduler
   * @todo delay one cycle for write.
   *       guarantee Write are Read is in a correct order.
   */
  val write = Queue(io.write, 1)

  /* Wiping the Directory with 0s on reset has ultimate priority. */
  val wipeCount = RegInit(UInt(0, width = params.setBits + 1))
  /** don't wipe tags during reset.
    * first cycle is false, then true.
    */
  val wipeOff = RegNext(Bool(false), Bool(true))
  /** signal indicate wiping is finished. */
  val wipeDone = wipeCount(params.setBits)
  /** signal indicate which set is wiping. */
  val wipeSet = wipeCount(params.setBits - 1,0)

  /* only ready after all set is clear. */
  io.ready := wipeDone
  when (!wipeDone && !wipeOff) { wipeCount := wipeCount + UInt(1) }
  assert (wipeDone || !io.read.valid)

  // Be explicit for dumb 1-port inference
  val ren = io.read.valid
  val wen = (!wipeDone && !wipeOff) || write.valid
  assert (!io.read.valid || wipeDone)

  require (codeBits <= 256)

  /* single port ram, write only not when not reading. */
  write.ready := !io.read.valid
  when (!ren && wen) {
    cc_dir.write(
      Mux(wipeDone, write.bits.set, wipeSet),
      Vec.fill(params.cache.ways) { Mux(wipeDone, write.bits.data.asUInt, UInt(0)) },
      UIntToOH(write.bits.way, params.cache.ways).asBools.map(_ || !wipeDone))
  }

  val ren1 = RegInit(Bool(false))
  val ren2 = if (params.micro.dirReg) RegInit(Bool(false)) else ren1
  ren2 := ren1
  ren1 := ren

  val bypass_valid = params.dirReg(write.valid)
  val bypass = params.dirReg(write.bits, ren1 && write.valid)
  val regout = params.dirReg(cc_dir.read(io.read.bits.set, ren), ren1)
  val tag = params.dirReg(RegEnable(io.read.bits.tag, ren), ren1)
  val set = params.dirReg(RegEnable(io.read.bits.set, ren), ren1)

  /* Compute the victim way in case of an evicition.
   * Random replacement
   * @note if use [[freechips.rocketchip.util.ReplacementPolicy]] it would be better.
   * @todo understand codes below.
   */
  val victimLFSR = LFSR16(params.dirReg(ren))(InclusiveCacheParameters.lfsrBits-1, 0)
  val victimSums = Seq.tabulate(params.cache.ways) { i => UInt((1 << InclusiveCacheParameters.lfsrBits)*i / params.cache.ways) }
  val victimLTE  = Cat(victimSums.map { _ <= victimLFSR }.reverse)
  val victimSimp = Cat(UInt(0, width=1), victimLTE(params.cache.ways-1, 1), UInt(1, width=1))
  val victimWayOH = victimSimp(params.cache.ways-1,0) & ~(victimSimp >> 1)
  val victimWay = OHToUInt(victimWayOH)
  assert (!ren2 || victimLTE(0) === UInt(1))
  assert (!ren2 || ((victimSimp >> 1) & ~victimSimp) === UInt(0)) // monotone
  assert (!ren2 || PopCount(victimWayOH) === UInt(1))

  /* if read didn't hit in SRAM, but hit data last cycle write to SRAM. */

  /** current cycle read match previous write. */
  val setQuash = bypass_valid && bypass.set === set
  /** */
  val tagMatch = bypass.data.tag === tag
  /** */
  val wayMatch = bypass.way === victimWay

  val ways = Vec(regout.map(d => new DirectoryEntry(params).fromBits(d)))
  val hits = Cat(ways.zipWithIndex.map { case (w, i) =>
    w.tag === tag && w.state =/= INVALID && (!setQuash || UInt(i) =/= bypass.way)
  }.reverse)
  val hit = hits.orR()

  io.result.valid := ren2
  io.result.bits := Mux(hit, Mux1H(hits, ways), Mux(setQuash && (tagMatch || wayMatch), bypass.data, Mux1H(victimWayOH, ways)))
  io.result.bits.hit := hit || (setQuash && tagMatch && bypass.data.state =/= INVALID)
  io.result.bits.way := Mux(hit, OHToUInt(hits), Mux(setQuash && tagMatch, bypass.way, victimWay))

  params.ccover(ren2 && setQuash && tagMatch, "DIRECTORY_HIT_BYPASS", "Bypassing write to a directory hit")
  params.ccover(ren2 && setQuash && !tagMatch && wayMatch, "DIRECTORY_EVICT_BYPASS", "Bypassing a write to a directory eviction")

  def json: String = s"""{"clients":${params.clientBits},"mem":"${cc_dir.pathName}","clean":"${wipeDone.pathName}"}"""
}
