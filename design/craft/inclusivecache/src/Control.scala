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

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._


import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

class InclusiveCacheControl(outer: InclusiveCache, control: InclusiveCacheControlParameters)(implicit p: Parameters) extends LazyModule()(p) {
  val ctrlnode = TLRegisterNode(
    address     = Seq(AddressSet(control.address, InclusiveCacheParameters.L2ControlSize-1)),
    device      = outer.device,
    concurrency = 1, // Only one flush command at a time (else need to track who answers)
    beatBytes   = control.beatBytes)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val nBanks = outer.ctrlBanks
    // The default cap leaves half of the A-capable MSHRs free for A requests during a flush command
    val maxInFlight = control.maxInFlight.getOrElse(
      ((InclusiveCacheParameters.all_mshrs(outer.cache, outer.micro) - 2) / 2) max 1)
    require (maxInFlight >= 1)

    val io = IO(new Bundle {
      val flush_match = Input(Vec(nBanks, Bool())) // The bank that contains `flush_req.bits`
      val flush_req = Decoupled(UInt(64.W))
      val invalidate_req = Output(Bool()) // Don't release block to main memory, just clear from directory
      val flush_resp = Input(Vec(nBanks, Bool()))
    })

    val blockBytes = outer.cache.blockBytes
    val lgBlockBytes = log2Ceil(blockBytes)

    // A flush command covers a range of blocks.
    // The generator sends each block as a separate request to the bank that contains the block.
    val busy = RegInit(false.B)
    val issuing = RegInit(false.B)
    val flushAddress = Reg(UInt(64.W))
    val flushLinesLeft = Reg(UInt(lgBlockBytes.W)) // Blocks left to issue, minus one
    val flushIsInvalidate = Reg(Bool())
    val inFlight = RegInit(VecInit(Seq.fill(nBanks)(0.U(log2Ceil(maxInFlight + 1).W))))

    val headMatch = io.flush_match.asUInt.orR
    val headAtCap = (io.flush_match zip inFlight).map { case (m, n) => m && n === maxInFlight.U }.reduce(_||_)
    io.flush_req.valid := issuing && headMatch && !headAtCap
    io.flush_req.bits := flushAddress
    io.invalidate_req := flushIsInvalidate

    // The generator skips a block that is outside every bank
    when (io.flush_req.fire || (issuing && !headMatch)) {
      flushAddress := flushAddress + blockBytes.U
      flushLinesLeft := flushLinesLeft - 1.U
      when (flushLinesLeft === 0.U) { issuing := false.B }
    }

    inFlight.zipWithIndex.foreach { case (n, i) =>
      assert (!io.flush_resp(i) || n =/= 0.U, "Flush response with no flush in flight")
      n := n + (io.flush_req.fire && io.flush_match(i)).asUInt - io.flush_resp(i).asUInt
    }

    val flushDone = busy && !issuing && !inFlight.map(_ =/= 0.U).reduce(_||_)

    def flushCommand(invalidate: Boolean, address: UInt => UInt, linesLeft: UInt => UInt) =
      RegWriteFn((ivalid, oready, data) => {
        when (ivalid && !busy) {
          busy := true.B
          issuing := true.B
          flushAddress := address(data)
          flushLinesLeft := linesLeft(data)
          flushIsInvalidate := invalidate.B
        }
        when (oready && flushDone) { busy := false.B }
        (!busy, flushDone)
      })

    def single(invalidate: Boolean, shift: Int) = flushCommand(invalidate, _ << shift, _ => 0.U)
    def range(invalidate: Boolean) =
      flushCommand(invalidate, data => Cat(data >> lgBlockBytes, 0.U(lgBlockBytes.W)), _(lgBlockBytes-1, 0))

    val flush32 = RegField.w(32, single(false, 4),
      RegFieldDesc("Flush32", "Flush the physical address equal to the 32-bit written data << 4 from the cache"))

    val flush64 = RegField.w(64, single(false, 0),
      RegFieldDesc("Flush64", "Flush the phsyical address equal to the 64-bit written data from the cache"))

    val invalidate32 = RegField.w(32, single(true, 4),
      RegFieldDesc("Invalidate32", "Invalidate the physical address equal to the 32-bit written data << 4 from the cache"))

    val invalidate64 = RegField.w(64, single(true, 0),
      RegFieldDesc("Invalidate64", "Invalidate the phsyical address equal to the 64-bit written data from the cache"))

    val flushRange64 = RegField.w(64, range(false),
      RegFieldDesc("FlushRange64", s"Flush (data[${lgBlockBytes-1}:0] + 1) consecutive blocks from the cache, " +
        s"starting at the physical address equal to the 64-bit written data with bits [${lgBlockBytes-1}:0] cleared. " +
        "A banked control port only flushes the blocks in its own bank"))

    val invalidateRange64 = RegField.w(64, range(true),
      RegFieldDesc("InvalidateRange64", s"Invalidate (data[${lgBlockBytes-1}:0] + 1) consecutive blocks from the cache, " +
        s"starting at the physical address equal to the 64-bit written data with bits [${lgBlockBytes-1}:0] cleared. " +
        "A banked control port only invalidates the blocks in its own bank"))

    // Information about the cache configuration
    val banksR  = RegField.r(8, outer.node.edges.in.size.U,         RegFieldDesc("Banks",
      "Number of banks in the cache", reset=Some(outer.node.edges.in.size)))
    val waysR   = RegField.r(8, outer.cache.ways.U,                 RegFieldDesc("Ways",
      "Number of ways per bank", reset=Some(outer.cache.ways)))
    val lgSetsR = RegField.r(8, log2Ceil(outer.cache.sets).U,       RegFieldDesc("lgSets",
      "Base-2 logarithm of the sets per bank", reset=Some(log2Ceil(outer.cache.sets))))
    val lgBlockBytesR = RegField.r(8, log2Ceil(outer.cache.blockBytes).U, RegFieldDesc("lgBlockBytes",
      "Base-2 logarithm of the bytes per cache block", reset=Some(log2Ceil(outer.cache.blockBytes))))

    val regmap = ctrlnode.regmap(
      0x000 -> RegFieldGroup("Config", Some("Information about the Cache Configuration"), Seq(banksR, waysR, lgSetsR, lgBlockBytesR)),
      0x200 -> (if (control.beatBytes >= 8) Seq(flush64) else Nil),
      0x240 -> Seq(flush32),
      0x280 -> (if (control.beatBytes >= 8) Seq(invalidate64) else Nil),
      0x2c0 -> Seq(invalidate32),
      0x300 -> (if (control.beatBytes >= 8) Seq(flushRange64) else Nil),
      0x340 -> (if (control.beatBytes >= 8) Seq(invalidateRange64) else Nil),
    )
  }
}
