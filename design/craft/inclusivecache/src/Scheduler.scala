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
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

/** A bank only have one scheduler.
  * allocate MSHR with request from A, C and X channel from clients.
  * Process task scheduled by MSHR.
  */
class Scheduler(params: InclusiveCacheParameters) extends Module
{
  val io = new Bundle {
    /** [[TLBundle]] from client to this cache. */
    val in = TLBundle(params.inner.bundle).flip
    /** [[TLBundle]] from this cache to manger. */
    val out = TLBundle(params.outer.bundle)
    /** @todo[code change] should be removed, since it is not used in InclusiveCache. */
    val ways = Vec(params.allClients, UInt(width = params.cache.ways)).flip
    val divs = Vec(params.allClients, UInt(width = InclusiveCacheParameters.lfsrBits + 1)).flip
    /** MMIO request for cache flush. */
    val req = Decoupled(new SinkXRequest(params)).flip
    /** MMIO response for cache flush. */
    val resp = Decoupled(new SourceXRequest(params))
  }

  // create sink and source and connect to correspond [[TLBundle]].
  val sourceA = Module(new SourceA(params))
  val sourceB = Module(new SourceB(params))
  val sourceC = Module(new SourceC(params))
  val sourceD = Module(new SourceD(params))
  val sourceE = Module(new SourceE(params))
  val sourceX = Module(new SourceX(params))

  io.out.a <> sourceA.io.a
  io.out.c <> sourceC.io.c
  io.out.e <> sourceE.io.e
  io.in.b <> sourceB.io.b
  io.in.d <> sourceD.io.d
  io.resp <> sourceX.io.x

  val sinkA = Module(new SinkA(params))
  val sinkC = Module(new SinkC(params))
  val sinkD = Module(new SinkD(params))
  val sinkE = Module(new SinkE(params))
  val sinkX = Module(new SinkX(params))

  sinkA.io.a <> io.in.a
  sinkC.io.c <> io.in.c
  sinkE.io.e <> io.in.e
  sinkD.io.d <> io.out.d
  sinkX.io.x <> io.req

  // tie down B channel connect from manager.
  io.out.b.ready := Bool(true)

  /** SRAM to store directory metadata. */
  val directory = Module(new Directory(params))
  /** SRAM to store cacheline. */
  val bankedStore = Module(new BankedStore(params))
  /** multiple linked buffer for 3 different MSHRs.
    * contents are requests from A, C channel.
    * @todo[code change] don't need to construct B MSHR.
    */
  val requests = Module(new ListBuffer(ListBufferParameters(new QueuedRequest(params), 3*params.mshrs, params.secondary, false)))
  /** Miss Status Handler Registers(MSHR) to process outstanding transactions.
    * preorder MSHR slot for the nested transactions to make sure
    * transaction on B channel can be nested in the A channel, and C will be nested in A and B.
    * 0 1 2 . . . n-1 n
    * x x x x x x       <- [[abc_mshrs]]
    *              x    <- [[bc_mshr]]
    *                 x <- [[c_mshr]]
    */
  val mshrs = Seq.fill(params.mshrs) { Module(new MSHR(params)) }
  /** MSHR available for all request. */
  val abc_mshrs = mshrs.init.init
  /** MSHR available for BC only request(don't allow A channel).
    * @todo[code change] remove this.
    */
  val bc_mshr = mshrs.init.last
  /** MSHR available for C only request(don't allow AB channel). */
  val c_mshr = mshrs.last
  /** for the transactions having same address.
    * a transaction from C Channel can be nested to A Channel.
    * This wire contains metadata changed by C Channel and will be broadcast to all MSHRs.
    */
  val nestedwb = Wire(new NestedWriteback(params))

  // Deliver messages from Sinks to MSHRs.
  mshrs.zipWithIndex.foreach { case (m, i) =>
    // valid when clients replying Probe by sending ProbeAck/ProbeAckData to SinkC.
    // It will update [[MSHR.probes_done]] and [[MSHR.probe_toN]].
    // Probe is sent because there is an outstanding transaction from A channel.
    // Since a transaction having a same set with a MSHR in [[mshrs]] and not being allowed to nest will be queued(see signal [[queue]]).
    // So only need to match set.
    // if Probe is sent, C won't be allowed to allocate MSHR.
    // if C is in the MSHR, A with same set will be stall, so cannot send a Probe, then cannot receive ProbeAck.
    m.io.sinkc.valid := sinkC.io.resp.valid && sinkC.io.resp.bits.set === m.io.status.bits.set
    // encode index of MSHR as source in D channel, sink in E channel.
    m.io.sinkd.valid := sinkD.io.resp.valid && sinkD.io.resp.bits.source === UInt(i)
    m.io.sinke.valid := sinkE.io.resp.valid && sinkE.io.resp.bits.sink   === UInt(i)
    // broadcast to each MSHR.
    m.io.sinkc.bits := sinkC.io.resp.bits
    m.io.sinkd.bits := sinkD.io.resp.bits
    m.io.sinke.bits := sinkE.io.resp.bits
    m.io.nestedwb := nestedwb
  }

  /** C MSHR will stall A MSHR with same set.
    * @todo[code change] remove B
    */
  val mshr_stall_abc = abc_mshrs.map { m =>
    (bc_mshr.io.status.valid && m.io.status.bits.set === bc_mshr.io.status.bits.set) ||
    ( c_mshr.io.status.valid && m.io.status.bits.set ===  c_mshr.io.status.bits.set)
  }
  /** @todo[code change] remove B */
  val mshr_stall_bc =
    c_mshr.io.status.valid && bc_mshr.io.status.bits.set === c_mshr.io.status.bits.set
  /** C won't be stall since it has highest priority. */
  val mshr_stall_c = Bool(false)
  /** concat all stall signals. */
  val mshr_stall = mshr_stall_abc :+ mshr_stall_bc :+ mshr_stall_c


  val stall_abc = (mshr_stall_abc zip abc_mshrs) map { case (s, m) => s && m.io.status.valid }
  if (!params.lastLevel || !params.firstLevel)
    params.ccover(stall_abc.reduce(_||_), "SCHEDULER_ABC_INTERLOCK", "ABC MSHR interlocked due to pre-emption")
  if (!params.lastLevel)
    params.ccover(mshr_stall_bc && bc_mshr.io.status.valid, "SCHEDULER_BC_INTERLOCK", "BC MSHR interlocked due to pre-emption")

  /** Find all MSHRs can be scheduled, only if all the resources required are available. */
  val mshr_request = Cat((mshrs zip mshr_stall).map { case (m, s) =>
    m.io.schedule.valid && !s &&
      (sourceA.io.req.ready || !m.io.schedule.bits.a.valid) &&
      (sourceB.io.req.ready || !m.io.schedule.bits.b.valid) &&
      (sourceC.io.req.ready || !m.io.schedule.bits.c.valid) &&
      (sourceD.io.req.ready || !m.io.schedule.bits.d.valid) &&
      (sourceE.io.req.ready || !m.io.schedule.bits.e.valid) &&
      (sourceX.io.req.ready || !m.io.schedule.bits.x.valid) &&
      (directory.io.write.ready || !m.io.schedule.bits.dir.valid)
  }.reverse)

  // Round-robin arbitration of MSHRs.
  // from right-hand-side, find the first one count from last selected.
  // When an MSHR wins the schedule, it has lowest priority next time.
  // for example:
  // [[mshr_request]]:         0010001000110000
  // last [[mshr_selectOH]]:   0000000010000000
  // [[robin_filter]]:         1111111100000000
  // [[robin_request]]:        0010001000110000 0010001000000000 // concat [[mshr_request]] and filtered [[mshr_request]].
  // [[mshr_selectOH2]]:       0000000000000000 0000001000000000 // find first one from right-hand-side.
  // [[mshr_selectOH]]:        0000001000000000 // and LSB and MSB of [[mshr_selectOH2]].

  val robin_filter = RegInit(UInt(0, width = params.mshrs))
  val robin_request = Cat(mshr_request, mshr_request & robin_filter)
  val mshr_selectOH2 = ~(leftOR(robin_request) << 1) & robin_request
  val mshr_selectOH = mshr_selectOH2(2*params.mshrs-1, params.mshrs) | mshr_selectOH2(params.mshrs-1, 0)
  val mshr_select = OHToUInt(mshr_selectOH)
  // select bits from [[mshrs]]
  val schedule = Mux1H(mshr_selectOH, mshrs.map(_.io.schedule.bits))
  val scheduleTag = Mux1H(mshr_selectOH, mshrs.map(_.io.status.bits.tag))
  val scheduleSet = Mux1H(mshr_selectOH, mshrs.map(_.io.status.bits.set))
  // update next [[mshr_request]]
  when (mshr_request.orR()) { robin_filter := ~rightOR(mshr_selectOH) }

  // Fill in which MSHR sends the request.
  schedule.a.bits.source := mshr_select
  // `schedule.c.bits.opcode(1)`: only set for Release/ReleaseData, not ProbeAck/ProbeAckData.
  // @todo why set 0 for ProbeAck/ProbeAckData,
  //       ProbeAck/ProbeAckData is routed by set match, when return to Scheduler,
  // @todo[code change] switch to AD only
  schedule.c.bits.source := Mux(schedule.c.bits.opcode(1), mshr_select, UInt(0))
  // mshr index to sink for E channel GrantAck sink match.
  schedule.d.bits.sink   := mshr_select

  // forward scheduler to correspond sources.
  sourceA.io.req := schedule.a
  sourceB.io.req := schedule.b
  sourceC.io.req := schedule.c
  sourceD.io.req := schedule.d
  sourceE.io.req := schedule.e
  sourceX.io.req := schedule.x
  directory.io.write := schedule.dir

  // Forward meta-data changes from nested transaction completion.
  // Nested transaction for metadata changing.
  val select_c  = mshr_selectOH(params.mshrs-1)
  // @todo[code change] remove B.
  val select_bc = mshr_selectOH(params.mshrs-2)
  // select source of nested writeback MSHR.
  // @todo[code change] connect to C directly.
  nestedwb.set   := Mux(select_c, c_mshr.io.status.bits.set, bc_mshr.io.status.bits.set)
  nestedwb.tag   := Mux(select_c, c_mshr.io.status.bits.tag, bc_mshr.io.status.bits.tag)
  // broadcast data to each MSHR to modify directory in each MSHR.
  // @todo[code change] remove B.
  nestedwb.b_toN       := select_bc && bc_mshr.io.schedule.bits.dir.valid && bc_mshr.io.schedule.bits.dir.bits.data.state === MetaData.INVALID
  nestedwb.b_toB       := select_bc && bc_mshr.io.schedule.bits.dir.valid && bc_mshr.io.schedule.bits.dir.bits.data.state === MetaData.BRANCH
  nestedwb.b_clr_dirty := select_bc && bc_mshr.io.schedule.bits.dir.valid
  nestedwb.c_set_dirty := select_c  &&  c_mshr.io.schedule.bits.dir.valid && c_mshr.io.schedule.bits.dir.bits.data.dirty

  /** selected highest priority request from C X A channel. */
  val request = Wire(Decoupled(new FullRequest(params)))
  // Directory is initiated, any of C X A channel has request.
  request.valid := directory.io.ready && (sinkA.io.req.valid || sinkX.io.req.valid || sinkC.io.req.valid)
  // priority select C > X > A
  request.bits := Mux(sinkC.io.req.valid, sinkC.io.req.bits,
                  Mux(sinkX.io.req.valid, sinkX.io.req.bits, sinkA.io.req.bits))
  sinkC.io.req.ready := directory.io.ready && request.ready
  sinkX.io.req.ready := directory.io.ready && request.ready && !sinkC.io.req.valid
  sinkA.io.req.ready := directory.io.ready && request.ready && !sinkC.io.req.valid && !sinkX.io.req.valid

  /** [[request]] match MSHR set in parallel. */
  val setMatches = Cat(mshrs.map { m => m.io.status.valid && m.io.status.bits.set === request.bits.set }.reverse)

  /** If no MSHR has been assigned to this set, we need to allocate one.
    * no matches also means no BC or C pre-emption on this set.
    */
  val alloc = !setMatches.orR()
  // If a same-set MSHR says that requests of this type must be blocked (for bounded time), do it.
  /** matched mshr by [[request]] will block manger B channel.
    * @todo[code change] remove B
    */
  val blockB = Mux1H(setMatches, mshrs.map(_.io.status.bits.blockB)) && request.bits.prio(1)
  /** matched mshr by [[request]] will block client C channel. */
  val blockC = Mux1H(setMatches, mshrs.map(_.io.status.bits.blockC)) && request.bits.prio(2)

  // If a same-set MSHR says that requests of this type must be handled out-of-band.
  // use special BC|C MSHR these special MSHRs interlock the MSHR that said it should be pre-empted.
  /** matched mshr by [[request]] will can be nested manger B channel.
    * @todo[code change] remove B
    */
  val nestB  = Mux1H(setMatches, mshrs.map(_.io.status.bits.nestB))  && request.bits.prio(1)
  /** matched mshr by [[request]] will can be nested client C channel. */
  val nestC  = Mux1H(setMatches, mshrs.map(_.io.status.bits.nestC))  && request.bits.prio(2)
  /** Prevent priority inversion, we may not queue to MSHRs beyond our level. */
  val prioFilter = Cat(request.bits.prio(2), !request.bits.prio(0), ~UInt(0, width = params.mshrs-2))
  /** MSHRs that this request can enter. */
  val lowerMatches = setMatches & prioFilter
  /** If we match an MSHR <= our priority that neither blocks nor nests us, queue to it. */
  val queue = lowerMatches.orR() && !nestB && !nestC && !blockB && !blockC

  // @todo[code change] remove lastLevel
  if (!params.lastLevel) {
    params.ccover(request.valid && blockB, "SCHEDULER_BLOCKB", "Interlock B request while resolving set conflict")
    params.ccover(request.valid && nestB,  "SCHEDULER_NESTB", "Priority escalation from channel B")
  }
  if (!params.firstLevel) {
    params.ccover(request.valid && blockC, "SCHEDULER_BLOCKC", "Interlock C request while resolving set conflict")
    params.ccover(request.valid && nestC,  "SCHEDULER_NESTC", "Priority escalation from channel C")
  }
  params.ccover(request.valid && queue, "SCHEDULER_SECONDARY", "Enqueue secondary miss")

  // It might happen that lowerMatches has >1 bit if the two special MSHRs are in-use
  // We want to queue to the highest matching priority MSHR.

  /** OneHot signal to indicate which MSHR can enter. */
  val lowerMatches1 =
    Mux(lowerMatches(params.mshrs-1), UInt(1 << (params.mshrs-1)),
    // @todo[code change] remove this.
    Mux(lowerMatches(params.mshrs-2), UInt(1 << (params.mshrs-2)),
    lowerMatches))

  // If this goes to the scheduled MSHR, it may need to be bypassed
  // Alternatively, the MSHR may be refilled from a request queued in the [[requests]]

  /** select request from queues. */
  val selected_requests = Cat(mshr_selectOH, mshr_selectOH, mshr_selectOH) & requests.io.valid
  /** Exists a request from A channel in [[requests]] is waiting to enter selected MSHR [[mshr_selectOH]]. */
  val a_pop = selected_requests((0 + 1) * params.mshrs - 1, 0 * params.mshrs).orR()
  /** Exists a request from B channel in [[requests]] is waiting to enter selected MSHR [[mshr_selectOH]].
    * @todo[code change] remove this.
    */
  val b_pop = selected_requests((1 + 1) * params.mshrs - 1, 1 * params.mshrs).orR()
  /** Exists a request from C channel in [[requests]] is waiting to enter selected MSHR [[mshr_selectOH]]. */
  val c_pop = selected_requests((2 + 1) * params.mshrs - 1, 2 * params.mshrs).orR()
  /** put request to correspond queue if queue is not empty, else put it to an empty MSHR.
    * C(queue) > C(request) > B(queue) > B(request) > A(queue) > A(request)
    */
  val bypassMatches = (mshr_selectOH & lowerMatches1).orR() &&
                      Mux(c_pop || request.bits.prio(2), !c_pop, Mux(b_pop || request.bits.prio(1), !b_pop, !a_pop))
  /** a valid request exists in queues. */
  val may_pop = a_pop || b_pop || c_pop
  /** this [[request]] will bypass queue, directly goes to MSHR. */
  val bypass = request.valid && queue && bypassMatches
  /** MSHR will reload a new request. */
  val will_reload = schedule.reload && (may_pop || bypass)
  /** queue will pop request to MSHR. */
  val will_pop = schedule.reload && may_pop && !bypass

  params.ccover(mshr_selectOH.orR && bypass, "SCHEDULER_BYPASS", "Bypass new request directly to conflicting MSHR")
  params.ccover(mshr_selectOH.orR && will_reload, "SCHEDULER_RELOAD", "Back-to-back service of two requests")
  params.ccover(mshr_selectOH.orR && will_pop, "SCHEDULER_POP", "Service of a secondary miss")


  /* @todo why not?
   * mshrs.zipWithIndex.map{ case (mshr, i) =>
   *   mshr.io.allocate.bits := Mux(bypass, Wire(new QueuedRequest(params), init = request.bits), requests.io.data)
   *   mshr.io.allocate.bits.set := mshr.io.status.bits.set
   *   mshr.io.allocate.bits.repeat := mshr.io.allocate.bits.tag === mshr.io.status.bits.tag
   *   mshr.io.allocate.valid := mshr_selectOH(i) && will_reload
   * }
   */
  // Repeat the above logic, but without the fan-in
  mshrs.zipWithIndex.foreach { case (m, i) =>
    val sel = mshr_selectOH(i)
    m.io.schedule.ready := sel
    val a_pop = requests.io.valid(params.mshrs * 0 + i)
    val b_pop = requests.io.valid(params.mshrs * 1 + i)
    val c_pop = requests.io.valid(params.mshrs * 2 + i)
    val bypassMatches = lowerMatches1(i) &&
                        Mux(c_pop || request.bits.prio(2), !c_pop, Mux(b_pop || request.bits.prio(1), !b_pop, !a_pop))
    val may_pop = a_pop || b_pop || c_pop
    val bypass = request.valid && queue && bypassMatches
    val will_reload = m.io.schedule.bits.reload && (may_pop || bypass)
    m.io.allocate.bits := Mux(bypass, Wire(new QueuedRequest(params), init = request.bits), requests.io.data)
    m.io.allocate.bits.set := m.io.status.bits.set
    // if tag match, MSHR won't access [[Directory]] for reloaded request.
    m.io.allocate.bits.repeat := m.io.allocate.bits.tag === m.io.status.bits.tag
    m.io.allocate.valid := sel && will_reload
  }

  // Determine which of the queued requests to pop (supposing will_pop)
  // use 3 fanin OR can to gate A with B and C, gate B with C.
  // @todo[code change] remove B.
  val prio_requests = ~(~requests.io.valid | (requests.io.valid >> params.mshrs) | (requests.io.valid >> 2*params.mshrs))
  /** [[requests]] index of queue to pop. */
  val pop_index = OHToUInt(Cat(mshr_selectOH, mshr_selectOH, mshr_selectOH) & prio_requests)
  // pop request from [[requests]]
  requests.io.pop.valid := will_pop
  requests.io.pop.bits  := pop_index

  // Reload from the Directory if the next MSHR operation changes tags.
  // if MSHR is able to reload, bypass [[request]] directly to MSHR, if [[requests]] is empty.

  // @todo[code change] code duplicated here.
  /** compare list buffer with current MSHR tag, if not match, prefetch from directory. */
  val lb_tag_mismatch = scheduleTag =/= requests.io.data.tag
  /** A request from [[requests]] may need access directory.
    * 1. in a bypass failed situation;
    * 2. request from [[requests]] is valid;
    * 3. tag mismatch(no [[MSHR.io.allocate.bits.repeat]]).
    */
  val mshr_uses_directory_assuming_no_bypass = schedule.reload && may_pop && lb_tag_mismatch
  /** access [[directory]] for the request popped from [[requests]]. */
  val mshr_uses_directory_for_lb = will_pop && lb_tag_mismatch
  /** access [[directory]] for the request popped from [[requests]] and bypassed from [[request]]. */
  val mshr_uses_directory = will_reload && scheduleTag =/= Mux(bypass, request.bits.tag, requests.io.data.tag)

  /** Is there an free MSHR for this request? */
  val mshr_validOH = Cat(mshrs.map(_.io.status.valid).reverse)
  /** based on [[request]], which MSHR is free. */
  val mshr_free = (~mshr_validOH & prioFilter).orR()

  /** [[request]] will bypass queue to correspond MSHR. */
  val bypassQueue = schedule.reload && bypassMatches

  // Fanout the request to the appropriate handler (if any).

  /** [[request]] will allocate a new MSHR.
    * @todo why !bc_mshr.io.status.valid && !c_mshr.io.status.valid?
    */
  val request_alloc_cases =
     // 1. request need to allocate a new MSHR
     // 2. can access directory
     // 3. a free MSHR.
     (alloc && !mshr_uses_directory_assuming_no_bypass && mshr_free) ||
     // @todo[code change] remove this
     (nestB && !mshr_uses_directory_assuming_no_bypass && !bc_mshr.io.status.valid && !c_mshr.io.status.valid) ||
     // 1. there is a lower priority MSHR with same set allowing new coming request to be nested.
     // 2. can access directory
     // 3. [[c_mshr]] is free.
     (nestC && !mshr_uses_directory_assuming_no_bypass && !c_mshr.io.status.valid)
  /** [[request]] will be consumed when allocated or queued.
    * 1. allocate to a free MSHR.
    * 2. enter to queue.
    * 3. bypass to a existed MSHR.
    */
  request.ready := request_alloc_cases || (queue && (bypassQueue || requests.io.push.ready))
  /** directory will be used by a new MSHR allocation. */
  val alloc_uses_directory = request.valid && request_alloc_cases

  // When a request goes through, it will need to hit the Directory.
  // situations to access directory:
  // 1. [[bypass]] or [[may_pop]] has same set but different tag.
  // 2. [[request_alloc_cases]] will allocate a new MSHR.
  directory.io.read.valid := mshr_uses_directory || alloc_uses_directory
  directory.io.read.bits.set := Mux(mshr_uses_directory_for_lb, scheduleSet,          request.bits.set)
  directory.io.read.bits.tag := Mux(mshr_uses_directory_for_lb, requests.io.data.tag, request.bits.tag)

  // Enqueue a valid request to [[requests]] if need to be queued and not being bypassed.
  requests.io.push.valid := request.valid && queue && !bypassQueue
  requests.io.push.bits.data  := request.bits
  requests.io.push.bits.index := Mux1H(
    request.bits.prio, Seq(
      OHToUInt(lowerMatches1 << params.mshrs*0),
      // @todo[code change] remove B.
      OHToUInt(lowerMatches1 << params.mshrs*1),
      OHToUInt(lowerMatches1 << params.mshrs*2)))

  /** find the highest priority free MSHR to insert request. */
  val mshr_insertOH = ~(leftOR(~mshr_validOH) << 1) & ~mshr_validOH & prioFilter

  (mshr_insertOH.asBools zip mshrs) map { case (s, m) =>
    // 1. request is valid.
    // 2. no same set MSHR is valid.
    // 3. @todo why not [[mshr_uses_directory_for_lb]]?
    when (request.valid && alloc && s && !mshr_uses_directory_assuming_no_bypass) {
      m.io.allocate.valid := Bool(true)
      m.io.allocate.bits := request.bits
      m.io.allocate.bits.repeat := Bool(false)
    }
  }

  // insert to [[bc_mshr]].
  // @todo[code change] remove this.
  when (request.valid && nestB && !bc_mshr.io.status.valid && !c_mshr.io.status.valid && !mshr_uses_directory_assuming_no_bypass) {
    bc_mshr.io.allocate.valid := Bool(true)
    bc_mshr.io.allocate.bits := request.bits
    bc_mshr.io.allocate.bits.repeat := Bool(false)
    assert (!request.bits.prio(0))
  }
  bc_mshr.io.allocate.bits.prio(0) := Bool(false)

  // insert a nested request to [[c_mshr]].
  when (request.valid && nestC && !c_mshr.io.status.valid && !mshr_uses_directory_assuming_no_bypass) {
    c_mshr.io.allocate.valid := Bool(true)
    c_mshr.io.allocate.bits := request.bits
    c_mshr.io.allocate.bits.repeat := Bool(false)
    assert (!request.bits.prio(0))
    assert (!request.bits.prio(1))
  }
  // @todo[code change] remove this.
  c_mshr.io.allocate.bits.prio(0) := Bool(false)
  c_mshr.io.allocate.bits.prio(1) := Bool(false)

  // Fanout the result of the Directory lookup.
  /** target directory to the MSHR need directory.
    * @todo[code change] remove B.
    */
  val dirTarget = Mux(alloc, mshr_insertOH, Mux(nestB, UInt(1 << (params.mshrs-2)), UInt(1 << (params.mshrs-1))))
  /** bits to indicate which MSHR need [[directory.io.result]]. */
  val directoryFanout = params.dirReg(RegNext(Mux(mshr_uses_directory,
    // MSHR selected because pop or bypass from [[requests]]
    mshr_selectOH,
    Mux(alloc_uses_directory,
      // MSHR selected because allocate from [[request]]
      dirTarget,
      // no MSHR need to access [[directory]](no request or [[MSHR.io.allocate.bits.repeat]])
      UInt(0)
    )
  )))
  mshrs.zipWithIndex.foreach { case (m, i) =>
    m.io.directory.valid := directoryFanout(i)
    m.io.directory.bits := directory.io.result.bits
  }

  // MSHR response meta-data fetch

  // sinkC receive ProbeAckData, query way from [[Scheduler]] by set.
  // [[Scheduler]] match set with MSHRs, get way for this ProbeAckData.
  // send way to [[sinkC]].
  // [[c_mshr]] only ReleaseData, no Probe to clients
  // @todo[code change] remove [[bc_mshr]]
  sinkC.io.way :=
    Mux(bc_mshr.io.status.valid && bc_mshr.io.status.bits.set === sinkC.io.set,
      bc_mshr.io.status.bits.way,
      Mux1H(abc_mshrs.map(m => m.io.status.valid && m.io.status.bits.set === sinkC.io.set),
            abc_mshrs.map(_.io.status.bits.way)))

  // use [[sinkD.io.source]] to find way and set
  sinkD.io.way := Vec(mshrs.map(_.io.status.bits.way))(sinkD.io.source)
  sinkD.io.set := Vec(mshrs.map(_.io.status.bits.set))(sinkD.io.source)

  // Beat buffer connections between components
  // [[sinkA]] PutData/PutPartialData/ArithmeticLogic/LogicalData
  sinkA.io.pb_pop <> sourceD.io.pb_pop
  sourceD.io.pb_beat := sinkA.io.pb_beat
  // [[sinkC]] ReleaseData
  sinkC.io.rel_pop <> sourceD.io.rel_pop
  sourceD.io.rel_beat := sinkC.io.rel_beat

  // Forward [[bankedStore]] ports
  bankedStore.io.sinkC_adr <> sinkC.io.bs_adr
  bankedStore.io.sinkC_dat := sinkC.io.bs_dat
  bankedStore.io.sinkD_adr <> sinkD.io.bs_adr
  bankedStore.io.sinkD_dat := sinkD.io.bs_dat
  bankedStore.io.sourceC_adr <> sourceC.io.bs_adr
  bankedStore.io.sourceD_radr <> sourceD.io.bs_radr
  bankedStore.io.sourceD_wadr <> sourceD.io.bs_wadr
  bankedStore.io.sourceD_wdat := sourceD.io.bs_wdat
  sourceC.io.bs_dat := bankedStore.io.sourceC_dat
  sourceD.io.bs_rdat := bankedStore.io.sourceD_rdat

  // Forward SourceD data hazard interlock
  sourceD.io.evict_req := sourceC.io.evict_req
  sourceD.io.grant_req := sinkD  .io.grant_req
  sourceC.io.evict_safe := sourceD.io.evict_safe
  sinkD  .io.grant_safe := sourceD.io.grant_safe

  private def afmt(x: AddressSet) = s"""{"base":${x.base},"mask":${x.mask}}"""
  private def addresses = params.inner.manager.managers.flatMap(_.address).map(afmt _).mkString(",")
  private def setBits = params.addressMapping.drop(params.offsetBits).take(params.setBits).mkString(",")
  private def tagBits = params.addressMapping.drop(params.offsetBits + params.setBits).take(params.tagBits).mkString(",")
  private def simple = s""""reset":"${reset.pathName}","tagBits":[${tagBits}],"setBits":[${setBits}],"blockBytes":${params.cache.blockBytes},"ways":${params.cache.ways}"""
  def json: String = s"""{"addresses":[${addresses}],${simple},"directory":${directory.json},"subbanks":${bankedStore.json}}"""
}
