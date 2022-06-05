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
import diplomaticobjectmodel.model.OMCacheMaster
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.InclusiveCacheLogicalTreeNode
import freechips.rocketchip.diplomaticobjectmodel.model.OMRegisterMap
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.BankedL2Key
import freechips.rocketchip.util._

/** [[InclusiveCache]] is a
  * 1. TileLink based.
  * 2. strictly inclusive.
  * 3. last-level cache(Probe by manager's B channel is not implemented).
  * 4. random replacement policy.
  *
  * Behavior:
  *
  * Message between Inclusive Cache and clients:
  *   1. A channel from clients:
  *     - PutFullData: @todo
  *     - PutPartialData: @todo
  *     - ArithmeticLogic, LogicalData:
  *       1. request to directory and MSHR, put request to [[SinkA.putbuffer]].
  *       1. directory delay 1 or 2 cycle([[InclusiveCacheMicroParameters.dirReg]]), send back to MSHR.
  *       1. when MSHR get message from directory, create a plan, scheduler at next cycle.
  *       1. scheduler will execute based on the plan:
  *         - client have blocks -> schedule Probe to client -> wait ProbeAck/ProbeAckData
  *         - not have enough permission -> AcquireBlock/AcquirePerm from manger -> wait Grant/GrantData
  *         - after all wait is done([[MSHR.no_wait]]), schedule [[SourceD]] to execute and write back metadata to [[Directory]] simultaneously.
  *         - [[SourceD]] is a pipeline has 4 stages:
  *           1. [[SourceD]] read [[BankedStore]] at first stage.
  *           1. [[SourceD]] read from [[SinkA.putbuffer]] at second stage.
  *           1. [[SourceD]] send TileLink message at third stage(not buffered).
  *           1. [[SourceD]] execute atomic operation and write [[BankedStore]] at fourth stage.
  *     - Get: @todo
  *     - Intent:
  *       1. request to directory and MSHR.
  *       1. directory delay 1 or 2 cycle([[InclusiveCacheMicroParameters.dirReg]]), send back to MSHR.
  *       1. when MSHR get message from directory, create a plan, schedule at next cycle.
  *       1. scheduler will execute based on the plan:
  *         - client have blocks and hint message is [[TLHints.PREFETCH_WRITE]] -> schedule Probe to client -> wait ProbeAck/ProbeAckData
  *         - not have enough permission -> AcquireBlock/AcquirePerm from manger -> wait Grant/GrantData
  *         - after all wait is done([[MSHR.no_wait]]), schedule [[SourceD]] to execute and write back metadata to [[Directory]] simultaneously.
  *         - [[SourceD]] is a pipeline has 3 stages:
  *           1. [[SourceD]] piped at first stage.
  *           1. [[SourceD]] piped at second stage.
  *           1. [[SourceD]] send TileLink message at third stage(not buffered).
  *     - AcquireBlock:
  *       1. request to directory and MSHR.
  *       1. directory delay 1 or 2 cycle([[InclusiveCacheMicroParameters.dirReg]]), send back to MSHR.
  *       1. when MSHR get message from directory, create a plan, schedule at next cycle.
  *       1. scheduler will execute based on the plan:
  *         - client have blocks and Acquire to Tip -> schedule Probe to client -> wait ProbeAck/ProbeAckData
  *         - not have enough permission -> AcquireBlock/AcquirePerm from manger -> wait Grant/GrantData
  *         - after [[MSHR.w_pprobeack]] and [[MSHR.w_grant]], schedule [[SourceD]] to execute
  *         - [[SourceD]] is a pipeline has 3 stages:
  *           1. [[SourceD]] read [[BankedStore]] at first stage.
  *           1. [[SourceD]] piped at second stage.
  *           1. [[SourceD]] send Grant to client at third stage(not buffered).
  *         - after [[MSHR.w_grantack]] write back metadata to [[Directory]] simultaneously.
  *     - AcquirePerm
  *       1. request to directory and MSHR.
  *       1. directory delay 1 or 2 cycle([[InclusiveCacheMicroParameters.dirReg]]), send back to MSHR.
  *       1. when MSHR get message from directory, create a plan, schedule at next cycle.
  *       1. scheduler will execute based on the plan:
  *         - client have blocks -> schedule Probe to client -> wait ProbeAck/ProbeAckData
  *         - not have enough permission -> AcquireBlock/AcquirePerm from manger -> wait Grant/GrantData
  *         - after [[MSHR.w_pprobeack]] and [[MSHR.w_grant]], schedule [[SourceD]] to execute
  *         - [[SourceD]] is a pipeline has 3 stages:
  *           1. [[SourceD]] piped at first stage.
  *           1. [[SourceD]] piped at second stage.
  *           1. [[SourceD]] send TileLink message at third stage(not buffered).
  *         - after [[MSHR.w_grantack]] write back metadata to [[Directory]] simultaneously.
  *   1. B channel to clients:
  *     - ProbeBlock: send to clients when [[MSHR.s_pprobe]] or [[MSHR.s_rprobe]]
  *     - PutFullData, PutPartialData, ArithmeticData, LogicalData, Get, Intent, ProbePerm: not supported.
  *   1. C channel from clients:
  *     - ProbeAck: modify [[MSHR.w_pprobeackfirst]] and [[MSHR.w_pprobeacklast]]
  *     - ProbeAckData: modify [[MSHR.w_pprobeackfirst]] and [[MSHR.w_pprobeacklast]]
  *     - Release:
  *       1. request to directory and MSHR.
  *       1. directory delay 1 or 2 cycle([[InclusiveCacheMicroParameters.dirReg]]), send back to MSHR.
  *       1. when MSHR get message from directory, create a plan, schedule at next cycle.
  *       1. scheduler will execute based on the plan:
  *         - after all wait is done([[MSHR.no_wait]]), schedule [[SourceD]] to execute and write back metadata to [[Directory]] simultaneously.
  *         - [[SourceD]] is a pipeline has 3 stages:
  *           1. [[SourceD]] piped at first stage.
  *           1. [[SourceD]] read at second stage.
  *           1. [[SourceD]] send TileLink message at third stage(not buffered).
  *     - ReleaseData:
  *       1. request to directory and MSHR.
  *       1. directory delay 1 or 2 cycle([[InclusiveCacheMicroParameters.dirReg]]), send back to MSHR.
  *       1. when MSHR get message from directory, create a plan, schedule at next cycle.
  *       1. scheduler will execute based on the plan:
  *         - after all wait is done([[MSHR.no_wait]]), schedule [[SourceD]] to execute and write back metadata to [[Directory]] simultaneously.
  *         - [[SourceD]] is a pipeline has 3 stages:
  *           1. [[SourceD]] piped at first stage.
  *           1. [[SourceD]] read `SinkC.putbuffer`.
  *           1. [[SourceD]] send TileLink message at third stage(not buffered).
  *           1. [[SourceD]] write [[BankedStore]] at fourth stage.
  *     - AccessAck, AccessAckData, HintAck: not supported.
  *   1. D channel to clients: @todo
  *     - AccessAck
  *     - AccessAckData
  *     - HintAck
  *     - Grant
  *     - GrantData
  *     - ReleaseAck
  *   1. E channel from clients:
  *     - GrantAck: modify [[MSHR.w_grantack]] to true.
  *
  */
class InclusiveCache(
  val cache: CacheParameters,
  val micro: InclusiveCacheMicroParameters,
  control: Option[InclusiveCacheControlParameters] = None
  )(implicit p: Parameters)
    extends LazyModule
{
  val access = TransferSizes(1, cache.blockBytes)
  val xfer = TransferSizes(cache.blockBytes, cache.blockBytes)
  val atom = TransferSizes(1, cache.beatBytes)

  var resourcesOpt: Option[ResourceBindings] = None

  val device: SimpleDevice = new SimpleDevice("cache-controller", Seq("sifive,inclusivecache0", "cache")) {
    def ofInt(x: Int) = Seq(ResourceInt(BigInt(x)))

    override def describe(resources: ResourceBindings): Description = {
      resourcesOpt = Some(resources)

      val Description(name, mapping) = super.describe(resources)
      // Find the outer caches
      val outer = node.edges.out
        .flatMap(_.manager.managers)
        .filter(_.supportsAcquireB)
        .flatMap(_.resources.headOption)
        .map(_.owner.label)
        .distinct
      val nextlevel: Option[(String, Seq[ResourceValue])] =
        if (outer.isEmpty) {
          None
        } else {
          Some("next-level-cache" -> outer.map(l => ResourceReference(l)).toList)
        }

      val extra = Map(
        "cache-level"            -> ofInt(2),
        "cache-unified"          -> Nil,
        "cache-size"             -> ofInt(cache.sizeBytes * node.edges.in.size),
        "cache-sets"             -> ofInt(cache.sets * node.edges.in.size),
        "cache-block-size"       -> ofInt(cache.blockBytes),
        "sifive,mshr-count"      -> ofInt(InclusiveCacheParameters.all_mshrs(cache, micro)))
      Description(name, mapping ++ extra ++ nextlevel)
    }
  }

  val node: TLAdapterNode = TLAdapterNode(
    clientFn  = { _ => TLClientPortParameters(Seq(TLClientParameters(
      name          = s"L${cache.level} InclusiveCache",
      sourceId      = IdRange(0, InclusiveCacheParameters.out_mshrs(cache, micro)),
      supportsProbe = xfer)))
    },
    managerFn = { m => TLManagerPortParameters(
      managers = m.managers.map { m => m.copy(
        regionType         = if (m.regionType >= RegionType.UNCACHED) RegionType.CACHED else m.regionType,
        resources          = Resource(device, "caches") +: m.resources,
        supportsAcquireB   = xfer,
        supportsAcquireT   = if (m.supportsAcquireT) xfer else TransferSizes.none,
        supportsArithmetic = if (m.supportsAcquireT) atom else TransferSizes.none,
        supportsLogical    = if (m.supportsAcquireT) atom else TransferSizes.none,
        supportsGet        = access,
        supportsPutFull    = if (m.supportsAcquireT) access else TransferSizes.none,
        supportsPutPartial = if (m.supportsAcquireT) access else TransferSizes.none,
        supportsHint       = access,
        alwaysGrantsT      = false,
        fifoId             = None)
      },
      beatBytes  = cache.beatBytes,
      endSinkId  = InclusiveCacheParameters.all_mshrs(cache, micro),
      minLatency = 2)
    })

  val ctlnode = control.map { c => TLRegisterNode(
    address     = Seq(AddressSet(c.address, 0xfff)),
    device      = device,
    concurrency = 1, // Only one flush at a time (else need to track who answers)
    beatBytes   = c.beatBytes)}

  lazy val module = new LazyModuleImp(this) {

    // If you have a control port, you must have at least one cache port
    require (!ctlnode.isDefined || !node.edges.in.isEmpty)

    // Extract the client IdRanges; must be the same on all ports!
    val clientIds = node.edges.in.headOption.map(_.client.clients.map(_.sourceId).sortBy(_.start))
    node.edges.in.foreach { e => require(e.client.clients.map(_.sourceId).sortBy(_.start) == clientIds.get) }

    // Use the natural ordering of clients (just like in Directory)
    node.edges.in.headOption.foreach { n =>
      println(s"L${cache.level} InclusiveCache Client Map:")
      n.client.clients.zipWithIndex.foreach { case (c,i) =>
        println(s"\t${i} <= ${c.name}")
      }
      println("")
    }

    def getMasters(): Seq[OMCacheMaster] = {
      // Use the natural ordering of clients (just like in Directory)
      node.edges.in.headOption.map { n =>
        n.client.clients.zipWithIndex.map {
          case (c, i) =>
            OMCacheMaster(
              id = i,
              documentationName = c.name
            )
        }
      }.getOrElse(Nil)
    }

    // Flush directive
    val flushInValid   = RegInit(Bool(false))
    val flushInReady   = Wire(init = Bool(false))
    val flushInAddress = Reg(UInt(width = 64))
    val flushNoMatch   = Wire(init = Bool(true))
    val flushOutValid  = RegInit(Bool(false))
    val flushOutReady  = Wire(init = Bool(false))

    when (flushOutReady) { flushOutValid := Bool(false) }
    when (flushInReady)  { flushInValid  := Bool(false) }

    when (flushNoMatch && flushInValid) {
      flushInReady := Bool(true)
      flushOutValid := Bool(true)
    }

    val flush32 = RegField.w(32, RegWriteFn((ivalid, oready, data) => {
      when (oready) { flushOutReady := Bool(true) }
      when (ivalid) { flushInValid := Bool(true) }
      when (ivalid && !flushInValid) { flushInAddress := data << 4 }
      (!flushInValid, flushOutValid)
    }), RegFieldDesc("Flush32", "Flush the physical address equal to the 32-bit written data << 4 from the cache"))

    val flush64 = RegField.w(64, RegWriteFn((ivalid, oready, data) => {
      when (oready) { flushOutReady := Bool(true) }
      when (ivalid) { flushInValid := Bool(true) }
      when (ivalid && !flushInValid) { flushInAddress := data }
      (!flushInValid, flushOutValid)
    }), RegFieldDesc("Flush64", "Flush the phsyical address equal to the 64-bit written data from the cache"))

    // Information about the cache configuration
    val banksR  = RegField.r(8, UInt(node.edges.in.size),               RegFieldDesc("Banks",
      "Number of banks in the cache", reset=Some(node.edges.in.size)))
    val waysR   = RegField.r(8, UInt(cache.ways),                       RegFieldDesc("Ways",
      "Number of ways per bank", reset=Some(cache.ways)))
    val lgSetsR = RegField.r(8, UInt(log2Ceil(cache.sets)),             RegFieldDesc("lgSets",
      "Base-2 logarithm of the sets per bank", reset=Some(log2Ceil(cache.sets))))
    val lgBlockBytesR = RegField.r(8, UInt(log2Ceil(cache.blockBytes)), RegFieldDesc("lgBlockBytes",
      "Base-2 logarithm of the bytes per cache block", reset=Some(log2Ceil(cache.blockBytes))))

    val regmap = ctlnode.map { c =>
      c.regmap(
        0x000 -> RegFieldGroup("Config", Some("Information about the Cache Configuration"), Seq(banksR, waysR, lgSetsR, lgBlockBytesR)),
        0x200 -> (if (control.get.beatBytes >= 8) Seq(flush64) else Seq()),
        0x240 -> Seq(flush32)
      )
    }

    // Create the L2 Banks
    val mods = (node.in zip node.out) map { case ((in, edgeIn), (out, edgeOut)) =>
      edgeOut.manager.managers.foreach { m =>
        require (m.supportsAcquireB.contains(xfer),
          s"All managers behind the L2 must support acquireB($xfer) " +
          s"but ${m.name} only supports (${m.supportsAcquireB})!")
        if (m.supportsAcquireT) require (m.supportsAcquireT.contains(xfer),
          s"Any probing managers behind the L2 must support acquireT($xfer) " +
          s"but ${m.name} only supports (${m.supportsAcquireT})!")
      }

      val params = InclusiveCacheParameters(cache, micro, control.isDefined, edgeIn, edgeOut)
      val scheduler = Module(new Scheduler(params))

      scheduler.io.in <> in
      out <> scheduler.io.out

      val flushSelect = edgeIn.manager.managers.flatMap(_.address).map(_.contains(flushInAddress)).reduce(_||_)
      when (flushSelect) { flushNoMatch := Bool(false) }

      when (flushSelect && scheduler.io.req.ready)  { flushInReady := Bool(true) }
      when (scheduler.io.resp.valid) { flushOutValid := Bool(true) }
      assert (!scheduler.io.resp.valid || flushSelect)

      scheduler.io.req.valid := flushInValid && flushSelect
      scheduler.io.req.bits.address := flushInAddress
      scheduler.io.resp.ready := !flushOutValid

      // Fix-up the missing addresses. We do this here so that the Scheduler can be
      // deduplicated by Firrtl to make hierarchical place-and-route easier.

      out.a.bits.address := params.restoreAddress(scheduler.io.out.a.bits.address)
      in .b.bits.address := params.restoreAddress(scheduler.io.in .b.bits.address)
      out.c.bits.address := params.restoreAddress(scheduler.io.out.c.bits.address)

      scheduler
    }

    def json = s"""{"banks":[${mods.map(_.json).mkString(",")}]"""
  }

  def logicalTreeNode: InclusiveCacheLogicalTreeNode = new InclusiveCacheLogicalTreeNode(
    device = device,
    cache = cache,
    micro = micro,
    nBanks = p(BankedL2Key).nBanks,
    node = node,
    regMap = module.regmap
  )
}
