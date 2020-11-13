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
import freechips.rocketchip.util.DescribedSRAM

import scala.math.{max, min}

/**
  *
  */
abstract class BankedStoreAddress(val inner: Boolean, params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  /** do not actually use the SRAMs, just block their use */
  val noop = Bool()
  val way  = UInt(width = params.wayBits)
  val set  = UInt(width = params.setBits)
  val beat = UInt(width = if (inner) params.innerBeatBits else params.outerBeatBits)
  val mask = UInt(width = if (inner) params.innerMaskBits else params.outerMaskBits)
}

trait BankedStoreRW
{
  val write = Bool()
}

class BankedStoreOuterAddress(params: InclusiveCacheParameters) extends BankedStoreAddress(false, params)
class BankedStoreInnerAddress(params: InclusiveCacheParameters) extends BankedStoreAddress(true, params)
class BankedStoreInnerAddressRW(params: InclusiveCacheParameters) extends BankedStoreInnerAddress(params) with BankedStoreRW

abstract class BankedStoreData(val inner: Boolean, params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val data = UInt(width = (if (inner) params.inner.manager.beatBytes else params.outer.manager.beatBytes)*8)
}

class BankedStoreOuterData(params: InclusiveCacheParameters) extends BankedStoreData(false, params)
class BankedStoreInnerData(params: InclusiveCacheParameters) extends BankedStoreData(true,  params)
class BankedStoreInnerPoison(params: InclusiveCacheParameters) extends BankedStoreInnerData(params)
class BankedStoreOuterPoison(params: InclusiveCacheParameters) extends BankedStoreOuterData(params)
class BankedStoreInnerDecoded(params: InclusiveCacheParameters) extends BankedStoreInnerData(params)
class BankedStoreOuterDecoded(params: InclusiveCacheParameters) extends BankedStoreOuterData(params)

/** assume a row has 32 banks, each bank has `beatBytes/portFactor` bytes.
  * row is the data structure which `portFactor` can support 4 outstanding access requests in pipeline.
  *
  * xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx <---- row: all subbanks has 4 stacks.
  *                                           port: "xxxxxxxx" beat will access these banks in a cycle.
  *
  *
  * request: set way beat mask -> subbank access?
  *
  * portFactor = 4
  * beatBytes = 64
  * rowBytes = beatBytes * portFactor = 4 * 64
  * writeBytes = 8                                                  -> width of a subbank.("x" in the graph)
  * numBanks = rowBytes /  writeBytes = 32
  * ports = beatBytes / params.micro.writeBytes = 64 / 8 = 8        -> how many subbanks should be accessed in each beat(client and manger might be different).
  * selectWidth = numBanks / ports = 4                              -> how many stacks of "xxxxxxxx"
  * bankBits = log(numBanks / ports) = 2
  *
  * way -> 0 set -> 0 beat -> 0 =>
  * index = 0                                                       -> index of banks(not shown in graph)
  * select = 0010                                                   -> which port to select
  * mask = 10101001                                                 -> mask from tilelink, can mask some subbanks to save power.
  *
  * 00000000 00000000 11111111 00000000    -> FillInterleaved(ports, select)
  * 10101001 10101001 10101001 10101001    -> Fill(numBanks/ports, mask)
  * 00000000 00000000 10101001 00000000    -> &
  *
  * bankBits -> log2(4)
  * set way ->
  * beat ->
  * mask ->
  *
  * `bankSel := Mux(b.valid, FillInterleaved(ports, select) & Fill(numBanks/ports, m), UInt(0))`
  *
  */
class BankedStore(params: InclusiveCacheParameters) extends Module
{
  val io = new Bundle {
    val sinkC_adr = Decoupled(new BankedStoreInnerAddress(params)).flip
    val sinkC_dat = new BankedStoreInnerPoison(params).flip
    val sinkD_adr = Decoupled(new BankedStoreOuterAddress(params)).flip
    val sinkD_dat = new BankedStoreOuterPoison(params).flip
    val sourceC_adr = Decoupled(new BankedStoreOuterAddress(params)).flip
    val sourceC_dat = new BankedStoreOuterDecoded(params)
    val sourceD_radr = Decoupled(new BankedStoreInnerAddress(params)).flip
    val sourceD_rdat = new BankedStoreInnerDecoded(params)
    val sourceD_wadr = Decoupled(new BankedStoreInnerAddress(params)).flip
    val sourceD_wdat = new BankedStoreInnerPoison(params).flip
  }

  /** clients beatBytes.
    * For each cycle how many bytes will be sent to clients.
    */
  val innerBytes = params.inner.manager.beatBytes
  /** manger beatBytes.
    * For each cycle how many bytes will be sent to mangers.
    */
  val outerBytes = params.outer.manager.beatBytes
  /** bytes size for each row. */
  val rowBytes = params.micro.portFactor * max(innerBytes, outerBytes)
  require (rowBytes < params.cache.sizeBytes)
  /** the depth of subbank SRAM. */
  val rowEntries = params.cache.sizeBytes / rowBytes
  /** width of request index. */
  val rowBits = log2Ceil(rowEntries)
  /** number of subbanks. */
  val numBanks = rowBytes / params.micro.writeBytes
  /** data width of subbank. */
  val codeBits = 8*params.micro.writeBytes

  /** subbanks */
  val cc_banks = Seq.tabulate(numBanks) {
    i =>
      DescribedSRAM(
        name = s"cc_banks_$i",
        desc = "Banked Store",
        size = rowEntries,
        data = UInt(width = codeBits)
      )
  }
  // These constraints apply on the port priorities:
  //  sourceC > sinkD     outgoing Release > incoming Grant      (we start eviction+refill concurrently)
  //  sinkC > sourceC     incoming ProbeAck > outgoing ProbeAck  (we delay probeack writeback by 1 cycle for QoR)
  //  sinkC > sourceDr    incoming ProbeAck > SourceD read       (we delay probeack writeback by 1 cycle for QoR)
  //  sourceDw > sourceDr modified data visible on next cycle    (needed to ensure SourceD forward progress)
  //  sinkC > sourceC     inner ProbeAck > outer ProbeAck        (make wormhole routing possible [not yet implemented])
  //  sinkC&D > sourceD*  beat arrival > beat read|update        (make wormhole routing possible [not yet implemented])

  // Combining these restrictions yields a priority scheme of:
  //  sinkC > sourceC > sinkD > sourceDw > sourceDr
  //          ^^^^^^^^^^^^^^^ outer interface

  // Requests have different port widths, but we don't want to allow cutting in line.
  // Suppose we have requests A > B > C requesting ports --A-, --BB, ---C.
  // The correct arbitration is to allow --A- only, not --AC.
  // Obviously --A-, BB--, ---C should still be resolved to BBAC.

  /** bundle to access [[cc_banks]]. */
  class Request extends Bundle {
    val wen      = Bool()
    val index    = UInt(width = rowBits)
    val bankSel  = UInt(width = numBanks)
    /** OR of all higher priority bankSels. */
    val bankSum  = UInt(width = numBanks)
    /** ports actually activated by request. */
    val bankEn   = UInt(width = numBanks)
    val data     = Vec(numBanks, UInt(width = codeBits))
  }

  /** Decode external access(way set beat mask) to [[cc_banks]].
    * @param b address with type [[BankedStoreAddress]].
    * @param write this request will write subbank.
    * @param d data of this beat.
    */
  def req[T <: BankedStoreAddress](b: DecoupledIO[T], write: Bool, d: UInt): Request = {
    /** beatBytes of this request. */
    val beatBytes = if (b.bits.inner) innerBytes else outerBytes
    /** for each cycle, how many subbanks will be accessed. */
    val ports = beatBytes / params.micro.writeBytes
    /** width of `numBanks / ports`. */
    val bankBits = log2Ceil(numBanks / ports)
    /** split data to subbanks. */
    val words = Seq.tabulate(ports) { i =>
      val data = d((i + 1) * 8 * params.micro.writeBytes - 1, i * 8 * params.micro.writeBytes)
      data
    }
    /** encode (way set beat) to subbank index. */
    val a = Cat(b.bits.way, b.bits.set, b.bits.beat)
    /** mask of this beat. */
    val m = b.bits.mask
    /** output [[Request]]. */
    val out = Wire(new Request)

    /** select a stack of subbank based on lower bits of [[a]]. */
    val select = UIntToOH(a(bankBits-1, 0), numBanks/ports)
    /** all ready signals of stack of subbanks.
      * `out.bankSum((i+1)*ports-1, i*ports) & m` -> each bits indicates correspond subbank is being accessed by higher priority request.
      * `!(out.bankSum((i+1)*ports-1, i*ports) & m).orR` -> this bits indicates correspond subbanks stack is ready.
      */
    val ready  = Cat(Seq.tabulate(numBanks/ports) { i => !(out.bankSum((i+1)*ports-1, i*ports) & m).orR } .reverse)
    /** select correspond substack from [[ready]]. */
    b.ready := ready(a(bankBits-1, 0))

    out.wen      := write
    /** higher bits in [[a]] decides index to access. */
    out.index    := a >> bankBits
    out.bankSel  := Mux(b.valid, FillInterleaved(ports, select) & Fill(numBanks/ports, m), UInt(0))
    /** block others in nested transaction. */
    out.bankEn   := Mux(b.bits.noop, UInt(0), out.bankSel & FillInterleaved(ports, ready))
    out.data     := Vec(Seq.fill(numBanks/ports) { words }.flatten)

    out
  }

  val innerData = UInt(0, width = innerBytes*8)
  val outerData = UInt(0, width = outerBytes*8)
  val W = Bool(true)
  val R = Bool(false)

  val sinkC_req    = req(io.sinkC_adr,    W, io.sinkC_dat.data)
  val sinkD_req    = req(io.sinkD_adr,    W, io.sinkD_dat.data)
  val sourceC_req  = req(io.sourceC_adr,  R, outerData)
  val sourceD_rreq = req(io.sourceD_radr, R, innerData)
  val sourceD_wreq = req(io.sourceD_wadr, W, io.sourceD_wdat.data)

  // See the comments above for why this prioritization is used
  val reqs = Seq(sinkC_req, sourceC_req, sinkD_req, sourceD_wreq, sourceD_rreq)

  // Connect priorities; note that even if a request does not go through due to failing
  // to obtain a needed subbank, it still blocks overlapping lower priority requests.
  reqs.foldLeft(UInt(0)) { case (sum, req) =>
    req.bankSum := sum
    req.bankSel | sum
  }

  /** SRAM access: 1 cycles read and latch to [[regout]], 1 cycle write. */
  val regout = Vec(cc_banks.zipWithIndex.map { case ((b, omSRAM), i) =>
    val en  = reqs.map(_.bankEn(i)).reduce(_||_)
    val sel = reqs.map(_.bankSel(i))
    val wen = PriorityMux(sel, reqs.map(_.wen))
    val idx = PriorityMux(sel, reqs.map(_.index))
    val data= PriorityMux(sel, reqs.map(_.data(i)))

    /* write */
    when (wen && en) { b.write(idx, data) }
    /* read */
    RegEnable(b.read(idx, !wen && en), RegNext(!wen && en))
  })

  /* align cycles between bankEn and sram read output. */
  val regsel_sourceC = RegNext(RegNext(sourceC_req.bankEn))
  val regsel_sourceD = RegNext(RegNext(sourceD_rreq.bankEn))

  /* Intentionally not Mux1H and/or an indexed-mux b/c we want it 0 when !sel to save decode power.(area)
   *
   * 00000000 00000000 00000000 xxxxxxxx -> regout masked bankEn
   * [[0], [0], [0], [hgfedcba]] -> group with stack.
   * [[0,0,0,h], ....[0,0,0, a]] -> transpose
   * [h, ..., a] -> reduce or
   * hgfedcba -> cat
   */

  val decodeC = regout.zipWithIndex.map {
    case (r, i) => Mux(regsel_sourceC(i), r, UInt(0))
  }.grouped(outerBytes/params.micro.writeBytes).toList.transpose.map(s => s.reduce(_|_))

  io.sourceC_dat.data := Cat(decodeC.reverse)

  val decodeD = regout.zipWithIndex.map {
    case (r, i) => Mux(regsel_sourceD(i), r, UInt(0))
  }.grouped(innerBytes/params.micro.writeBytes).toList.transpose.map(s => s.reduce(_|_))

  io.sourceD_rdat.data := Cat(decodeD.reverse)

  private def banks = cc_banks.map("\"" + _._1.pathName + "\"").mkString(",")
  def json: String = s"""{"widthBytes":${params.micro.writeBytes},"mem":[${banks}]}"""
}
