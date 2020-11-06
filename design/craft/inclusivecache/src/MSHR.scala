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
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.tilelink._
import TLPermissions._
import TLMessages._
import MetaData._

class ScheduleRequest(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val a = Valid(new SourceARequest(params))
  val b = Valid(new SourceBRequest(params))
  val c = Valid(new SourceCRequest(params))
  val d = Valid(new SourceDRequest(params))
  val e = Valid(new SourceERequest(params))
  val x = Valid(new SourceXRequest(params))
  val dir = Valid(new DirectoryWrite(params))
  val reload = Bool() // get next request via allocate (if any)
}

class MSHRStatus(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val set = UInt(width = params.setBits)
  val tag = UInt(width = params.tagBits)
  val way = UInt(width = params.wayBits)
  /** block Probe from B channel by a Manager(Probe)
    * @todo if last level, there are no Probe from Manager.
    */
  val blockB = Bool()
  /** If a Probe from mangers comes to this Cache,
    * it will access both directory and MSHR.
    * [[nestB]] means this MSHR can be Probed this Probe.
    */
  val nestB  = Bool()
  /**  */
  val blockC = Bool()
  val nestC  = Bool()
}

class NestedWriteback(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val set = UInt(width = params.setBits)
  val tag = UInt(width = params.tagBits)
  val b_toN       = Bool() // nested Probes may unhit us
  val b_toB       = Bool() // nested Probes may demote us
  val b_clr_dirty = Bool() // nested Probes clear dirty
  val c_set_dirty = Bool() // nested Releases MAY set dirty
}

sealed trait CacheState
{
  val code = UInt(CacheState.index)
  CacheState.index = CacheState.index + 1
}

object CacheState
{
  var index = 0
}

case object S_INVALID  extends CacheState
case object S_BRANCH   extends CacheState
case object S_BRANCH_C extends CacheState
case object S_TIP      extends CacheState
case object S_TIP_C    extends CacheState
case object S_TIP_CD   extends CacheState
case object S_TIP_D    extends CacheState
case object S_TRUNK_C  extends CacheState
case object S_TRUNK_CD extends CacheState

/** Miss Status Handling Register of InclusiveCache.
  *
  * a MSHR will track address, data, and status for a memory access related to coherence.
  *
  */
class MSHR(params: InclusiveCacheParameters) extends Module
{
  val io = new Bundle {
    /** request from scheduler to allocate this MSHR for a transaction.
      *
      * refills this MSHR for next cycle.
      */
    val allocate  = Valid(new AllocateRequest(params)).flip
    /** result of querying directory, will be store to [[meta]] via [[new_meta]].
      *
      * it will trigger schedule setup.
      */
    val directory = Valid(new DirectoryResult(params)).flip
    /** Status of this MSHR. */
    val status    = Valid(new MSHRStatus(params))
    /** Interface to Sources to react upon current transaction. */
    val schedule  = Decoupled(new ScheduleRequest(params))
    /** Receive response from Sink C,
      * ProbeAck, ProbeAckData
      */
    val sinkc     = Valid(new SinkCResponse(params)).flip
    /** Receive response from Sink D,
      * Grant, GrantData, ReleaseAck
      */
    val sinkd     = Valid(new SinkDResponse(params)).flip
    /** Receive response from Sink E.
      * GrantAck
      */
    val sinke     = Valid(new SinkEResponse(params)).flip
    /** During the lifttime of a MSHR,
      * a nested transaction(Probe from Manager or Release from Client) could happen,
      * which modify status this cacheline.
      */
    val nestedwb  = new NestedWriteback(params).flip
  }
  /** allocate when new transaction comes. */
  val request_valid = RegInit(Bool(false))
  /** register to store request from [[io.allocate.bits]]. */
  val request = Reg(new FullRequest(params))
  /** wait for directory, when it comes, this become `true.B`. */
  val meta_valid = RegInit(Bool(false))
  /** This is original directory at the beginning of request comes.
    * But it can be overwritten by nested transaction.
    */
  val meta = Reg(new DirectoryResult(params))

  // Define which states are valid
  when (meta_valid) {
    when (meta.state === INVALID) {
      assert (!meta.clients.orR)
      assert (!meta.dirty)
    }
    when (meta.state === BRANCH) {
      assert (!meta.dirty)
    }
    when (meta.state === TRUNK) {
      assert (meta.clients.orR)
      /* have and only have one client. */
      assert ((meta.clients & (meta.clients - UInt(1))) === UInt(0))
    }
    when (meta.state === TIP) {
      // noop
    }
  }

  /* Completed transitions (s_ = scheduled), (w_ = waiting)
   * rprobe -> probe invoked by release.
   * pprobe -> probe invoked by permission transferring.
   */

  val s_rprobe         = RegInit(Bool(true)) // B
  /** Release ->
    * cacheline eviction ->
    * Probe clients
    * ture -> done
    * false -> to be done
    */
  val w_rprobeackfirst = RegInit(Bool(true))

  val w_rprobeacklast  = RegInit(Bool(true))
  /** schedule release to client. */
  val s_release        = RegInit(Bool(true)) // CW w_rprobeackfirst
  /** wait for release ack from manager. */
  val w_releaseack     = RegInit(Bool(true))
  /** schedule pprobe to client */
  val s_pprobe         = RegInit(Bool(true)) // B
  /** schedule acquire to manger. */
  val s_acquire        = RegInit(Bool(true)) // A  s_release, s_pprobe [1]
  /** schedule flush to manger. */
  val s_flush          = RegInit(Bool(true)) // X  w_releaseack
  /** got grantfirst. */
  val w_grantfirst     = RegInit(Bool(true))
  /** got the last beats of grant. */
  val w_grantlast      = RegInit(Bool(true))
  /** if true, Grant is done. */
  val w_grant          = RegInit(Bool(true)) // first | last depending on wormhole
  val w_pprobeackfirst = RegInit(Bool(true))
  val w_pprobeacklast  = RegInit(Bool(true))
  /** if true, ProbeAck is done. */
  val w_pprobeack      = RegInit(Bool(true)) // first | last depending on wormhole
  val s_probeack       = RegInit(Bool(true)) // C  w_pprobeackfirst (mutually exclusive with next two s_*)
  val s_grantack       = RegInit(Bool(true)) // E  w_grantfirst ... CAN require both outE&inD to service outD
  /** [[w_pprobeack]] and [[w_grant]] is done. */
  val s_execute        = RegInit(Bool(true)) // D  w_pprobeack, w_grant
  /** wait for GrantAck from Client. */
  val w_grantack       = RegInit(Bool(true))
  /** schedule writeback to directory. */
  val s_writeback      = RegInit(Bool(true)) // W  w_*

  // [1]: We cannot issue outer Acquire while holding blockB (=> outA can stall)
  // However, inB and outC are higher priority than outB, so s_release and s_pprobe
  // may be safely issued while blockB. Thus we must NOT try to schedule the
  // potentially stuck s_acquire with either of them (scheduler is all or none).

  /* Meta-data that we discover underway */
  /** cache sink from D channel to manger, match E channel from manger with this register. */
  val sink = Reg(UInt(width = params.outer.bundle.sinkBits))
  /** manger give T to this level. */
  val gotT = Reg(Bool())
  /** Acquire is rejected by manger. */
  val bad_grant = Reg(Bool())
  val probes_done = Reg(UInt(width = params.clientBits))
  val probes_toN = Reg(UInt(width = params.clientBits))
  val probes_noT = Reg(Bool())

  /* When a nested transaction(Probe or eviction) completes, update meta data in MSHR. */
  when (meta_valid && meta.state =/= INVALID &&
        io.nestedwb.set === request.set && io.nestedwb.tag === meta.tag) {
    /* Dirty has been probed by a manager. */
    when (io.nestedwb.b_clr_dirty) { meta.dirty := Bool(false) }
    /* A Client release data to this level. */
    when (io.nestedwb.c_set_dirty) { meta.dirty := Bool(true) }
    /* A manger Probe this cacheline to BRANCH. */
    when (io.nestedwb.b_toB) { meta.state := BRANCH }
    /* A manger Probe this cacheline to INVALID. */
    when (io.nestedwb.b_toN) { meta.hit := Bool(false) }
  }

  // status send to Scheduler
  /* This MSHR is processing data. */
  io.status.valid := request_valid
  io.status.bits.set    := request.set
  io.status.bits.tag    := request.tag
  /* way location is from directory. */
  io.status.bits.way    := meta.way

  /* Handle concurrent priority among TileLink channels.
   * block: There are lower priority outstanding transactions,
   *        and higher priority outstanding transactions,
   *        So the nest transaction is blocked.
   * nest: There are lower priority outstanding transactions,
   *       and no higher priority outstanding transactions,
   *       So the nest transaction is allowed.
   * queue: There are no lower priority outstanding transactions,
   *        So there are no nest transactions.
   */
  /* `!meta_valid`: nothing can be probed here, since MSHR has no directory currently.
   * `((!w_releaseack || !w_rprobeacklast || !w_pprobeacklast) && !w_grantfirst)`:
   *   `(!w_releaseack || !w_rprobeacklast || !w_pprobeacklast)` all C channel from clients related transaction is processing.
   *   `!w_grantfirst`: A Acquire(miss Data/Permission) to manager is outstanding.
   */
  io.status.bits.blockB := !meta_valid || ((!w_releaseack || !w_rprobeacklast || !w_pprobeacklast) && !w_grantfirst)
  /* `meta_valid`: the directory is valid in this MSHR.
   * w_releaseack && w_rprobeacklast && w_pprobeacklast && !w_grantfirst:
   *   `w_releaseack && w_rprobeacklast && w_pprobeacklast`: all C channel related transaction has been processed.
   *   `!w_grantfirst`: A Acquire(miss Data/Permission) to manager is outstanding
   */
  io.status.bits.nestB  := meta_valid && w_releaseack && w_rprobeacklast && w_pprobeacklast && !w_grantfirst
  // The above rules ensure we will block and not nest an outer probe while still doing our own inner probes.
  // Thus every probe wakes exactly one MSHR.

  io.status.bits.blockC := !meta_valid
  io.status.bits.nestC  := meta_valid && (!w_rprobeackfirst || !w_pprobeackfirst || !w_grantfirst)
  // The w_grantfirst in nestC is necessary to deal with:
  //   acquire waiting for grant, inner release gets queued, outer probe -> inner probe -> deadlock
  // ... this is possible because the release+probe can be for same set, but different tag

  // We can only demand: block, nest, or queue
  assert (!io.status.bits.nestB || !io.status.bits.blockB)
  assert (!io.status.bits.nestC || !io.status.bits.blockC)

  /** there are nothing to wait, the end of MSHR lifetime. */
  val no_wait = w_rprobeacklast && w_releaseack && w_grantlast && w_pprobeacklast && w_grantack

  /** Based on the status registers, set valid signal for scheduler. */
  io.schedule.bits.a.valid := !s_acquire && s_release && s_pprobe
  io.schedule.bits.b.valid := !s_rprobe || !s_pprobe
  io.schedule.bits.c.valid := (!s_release && w_rprobeackfirst) || (!s_probeack && w_pprobeackfirst)
  io.schedule.bits.d.valid := !s_execute && w_pprobeack && w_grant
  io.schedule.bits.e.valid := !s_grantack && w_grantfirst
  io.schedule.bits.x.valid := !s_flush && w_releaseack

  /* write back directory.
   * (!s_release && w_rprobeackfirst): if this writeback is by Release, writeback metadata after getting ProbeAck,
   *                                   this writeback occurred early than the death of this MSHR.
   */
  io.schedule.bits.dir.valid := (!s_release && w_rprobeackfirst) || (!s_writeback && no_wait)
  io.schedule.bits.reload := no_wait
  io.schedule.valid := io.schedule.bits.a.valid || io.schedule.bits.b.valid || io.schedule.bits.c.valid ||
                       io.schedule.bits.d.valid || io.schedule.bits.e.valid || io.schedule.bits.x.valid ||
                       io.schedule.bits.dir.valid

  /* Schedule completions will modify status registers.
   * [[io.schedule.ready]] indicate, scheduler is already accepted request from MSHR.
   */

  when (io.schedule.ready) {
                                    s_rprobe     := Bool(true)
    when (w_rprobeackfirst)       { s_release    := Bool(true) }
                                    s_pprobe     := Bool(true)
    when (s_release && s_pprobe)  { s_acquire    := Bool(true) }
    when (w_releaseack)           { s_flush      := Bool(true) }
    when (w_pprobeackfirst)       { s_probeack   := Bool(true) }
    when (w_grantfirst)           { s_grantack   := Bool(true) }
    when (w_pprobeack && w_grant) { s_execute    := Bool(true) }
    when (no_wait)                { s_writeback  := Bool(true) }
    // Await the next operation
    when (no_wait) {
      request_valid := Bool(false)
      meta_valid := Bool(false)
    }
  }

  /** metadata need to writeback to directory. */
  val final_meta_writeback = Wire(init = meta)

  /** which client this source belong to? */
  val req_clientBit = params.clientBit(request.source)
  /** is this request need a TIP permission? */
  val req_needT = needT(request.opcode, request.param)
  /** is this request a Acquire? */
  val req_acquire = request.opcode === AcquireBlock || request.opcode === AcquirePerm
  /** This cacheline are not existed in any clients. */
  val meta_no_clients = !meta.clients.orR
  /** can give T to this Acquire request. */
  val req_promoteT = req_acquire && Mux(meta.hit, meta_no_clients && meta.state === TIP, gotT)

  /** Request from C channel
    * Handel Release or ReleaseData.
    */
  when (request.prio(2) && Bool(!params.firstLevel)) { // always a hit
    /* dirty if cache is already dirty or receive ReleaseData. */
    final_meta_writeback.dirty   := meta.dirty || request.opcode(0)
    /* If this is a TIP or BRANCH, no matter what client release, state don't change.
     * If this is a TRUNK, this state will become TIP, if client release its TIP.
     */
    final_meta_writeback.state   := Mux(request.param =/= TtoT && meta.state === TRUNK, TIP, meta.state)
    /* Remove client to metadata if this request is ToN. */
    final_meta_writeback.clients := meta.clients & ~Mux(isToN(request.param), req_clientBit, UInt(0))
    /* always hit since this is the inclusive chache. */
    final_meta_writeback.hit     := Bool(true) // chained requests are hits
  } .elsewhen (request.control && Bool(params.control)) { // request.prio(0)
    when (meta.hit) {
      final_meta_writeback.dirty   := Bool(false)
      final_meta_writeback.state   := INVALID
      final_meta_writeback.clients := meta.clients & ~probes_toN
    }
    final_meta_writeback.hit := Bool(false)
  } .otherwise {
    final_meta_writeback.dirty := (meta.hit && meta.dirty) || !request.opcode(2)
    final_meta_writeback.state := Mux(req_needT,
                                    Mux(req_acquire, TRUNK, TIP),
                                    Mux(!meta.hit, Mux(gotT, Mux(req_acquire, TRUNK, TIP), BRANCH),
                                      MuxLookup(meta.state, UInt(0, width=2), Seq(
                                        INVALID -> BRANCH,
                                        BRANCH  -> BRANCH,
                                        TRUNK   -> TIP,
                                        TIP     -> Mux(meta_no_clients && req_acquire, TRUNK, TIP)))))
    final_meta_writeback.clients := Mux(meta.hit, meta.clients & ~probes_toN, UInt(0)) |
                                    Mux(req_acquire, req_clientBit, UInt(0))
    final_meta_writeback.tag := request.tag
    final_meta_writeback.hit := Bool(true)
  }

  when (bad_grant) {
    when (meta.hit) {
      /** upgrade failed (B -> T)
        * Only occurs when this cacheline has metadata but is a branch.
        * So acquire only can be AcquirePerm.
        */
      assert (!meta_valid || meta.state === BRANCH)
      final_meta_writeback.hit     := Bool(true)
      final_meta_writeback.dirty   := Bool(false)
      final_meta_writeback.state   := BRANCH
      final_meta_writeback.clients := meta.clients & ~probes_toN
    } .otherwise {
      /* upgrade failed from N */
      final_meta_writeback.hit     := Bool(false)
      final_meta_writeback.dirty   := Bool(false)
      final_meta_writeback.state   := INVALID
      final_meta_writeback.clients := UInt(0)
    }
  }
  /* A invalid Directory. */
  val invalid = Wire(new DirectoryEntry(params))
  invalid.dirty   := Bool(false)
  invalid.state   := INVALID
  invalid.clients := UInt(0)
  invalid.tag     := UInt(0)

  /** Just because a client says BtoT, by the time we process the request he may be N.
    * Therefore, we must consult our own meta-data state to confirm he owns the line still.
    * Client ask for BtoT, but Probed by nested transactions, this will detect if client is still BRANCH to decide NtoT or BtoT to reply.
    */
  val honour_BtoT = meta.hit && (meta.clients & req_clientBit).orR

  /** The client asking us to act is proof they don't have permissions.
    * Exclude client send this request.
    * `request.prio(0)` request is from A channel.
    */
  val excluded_client = Mux(meta.hit && request.prio(0) && skipProbeN(request.opcode), req_clientBit, UInt(0))

  io.schedule.bits.a.bits.tag     := request.tag
  io.schedule.bits.a.bits.set     := request.set
  io.schedule.bits.a.bits.param   := Mux(req_needT, Mux(meta.hit, BtoT, NtoT), NtoB)
  io.schedule.bits.a.bits.block   := request.size =/= UInt(log2Ceil(params.cache.blockBytes)) ||
                                     !(request.opcode === PutFullData || request.opcode === AcquirePerm)
  io.schedule.bits.a.bits.source  := UInt(0)
  io.schedule.bits.b.bits.param   := Mux(!s_rprobe, toN, Mux(request.prio(1), request.param, Mux(req_needT, toN, toB)))
  io.schedule.bits.b.bits.tag     := Mux(!s_rprobe, meta.tag, request.tag)
  io.schedule.bits.b.bits.set     := request.set
  io.schedule.bits.b.bits.clients := meta.clients & ~excluded_client
  io.schedule.bits.c.bits.opcode  := Mux(meta.dirty, ReleaseData, Release)
  io.schedule.bits.c.bits.param   := Mux(meta.state === BRANCH, BtoN, TtoN)
  io.schedule.bits.c.bits.source  := UInt(0)
  io.schedule.bits.c.bits.tag     := meta.tag
  io.schedule.bits.c.bits.set     := request.set
  io.schedule.bits.c.bits.way     := meta.way
  io.schedule.bits.c.bits.dirty   := meta.dirty
  io.schedule.bits.d.bits         := request
  io.schedule.bits.d.bits.param   := Mux(!req_acquire, request.param,
                                       MuxLookup(request.param, Wire(request.param), Seq(
                                         NtoB -> Mux(req_promoteT, NtoT, NtoB),
                                         BtoT -> Mux(honour_BtoT,  BtoT, NtoT),
                                         NtoT -> NtoT)))
  io.schedule.bits.d.bits.sink    := UInt(0)
  io.schedule.bits.d.bits.way     := meta.way
  io.schedule.bits.d.bits.bad     := bad_grant
  io.schedule.bits.e.bits.sink    := sink
  io.schedule.bits.x.bits.fail    := Bool(false)
  io.schedule.bits.dir.bits.set   := request.set
  io.schedule.bits.dir.bits.way   := meta.way
  io.schedule.bits.dir.bits.data  := Mux(!s_release, invalid, Wire(new DirectoryEntry(params), init = final_meta_writeback))

  // Coverage of state transitions
  def cacheState(entry: DirectoryEntry, hit: Bool) = {
    val out = Wire(UInt())
    val c = entry.clients.orR
    val d = entry.dirty
    switch (entry.state) {
      is (BRANCH)  { out := Mux(c, S_BRANCH_C.code, S_BRANCH.code) }
      is (TRUNK)   { out := Mux(d, S_TRUNK_CD.code, S_TRUNK_C.code) }
      is (TIP)     { out := Mux(c, Mux(d, S_TIP_CD.code, S_TIP_C.code), Mux(d, S_TIP_D.code, S_TIP.code)) }
      is (INVALID) { out := S_INVALID.code }
    }
    when (!hit) { out := S_INVALID.code }
    out
  }

  val p = !params.lastLevel  // can be probed
  val c = !params.firstLevel // can be acquired
  val m = params.inner.client.clients.exists(!_.supports.probe)   // can be written (or read)
  val r = params.outer.manager.managers.exists(!_.alwaysGrantsT) // read-only devices exist
  val f = params.control     // flush control register exists
  val cfg = (p, c, m, r, f)
  val b = r || p // can reach branch state (via probe downgrade or read-only device)

  // The cache must be used for something or we would not be here
  require(c || m)

  val evict = cacheState(meta, !meta.hit)
  val before = cacheState(meta, meta.hit)
  val after  = cacheState(final_meta_writeback, Bool(true))

  def eviction(from: CacheState, cover: Boolean)(implicit sourceInfo: SourceInfo) {
    if (cover) {
      params.ccover(evict === from.code, s"MSHR_${from}_EVICT", s"State transition from ${from} to evicted ${cfg}")
    } else {
      assert(!(evict === from.code), s"State transition from ${from} to evicted should be impossible ${cfg}")
    }
    if (cover && f) {
      params.ccover(before === from.code, s"MSHR_${from}_FLUSH", s"State transition from ${from} to flushed ${cfg}")
    } else {
      assert(!(before === from.code), s"State transition from ${from} to flushed should be impossible ${cfg}")
    }
  }

  def transition(from: CacheState, to: CacheState, cover: Boolean)(implicit sourceInfo: SourceInfo) {
    if (cover) {
      params.ccover(before === from.code && after === to.code, s"MSHR_${from}_${to}", s"State transition from ${from} to ${to} ${cfg}")
    } else {
      assert(!(before === from.code && after === to.code), s"State transition from ${from} to ${to} should be impossible ${cfg}")
    }
  }

  when ((!s_release && w_rprobeackfirst) && io.schedule.ready) {
    eviction(S_BRANCH,    b)      // MMIO read to read-only device
    eviction(S_BRANCH_C,  b && c) // you need children to become C
    eviction(S_TIP,       true)   // MMIO read || clean release can lead to this state
    eviction(S_TIP_C,     c)      // needs two clients || client + mmio || downgrading client
    eviction(S_TIP_CD,    c)      // needs two clients || client + mmio || downgrading client
    eviction(S_TIP_D,     true)   // MMIO write || dirty release lead here
    eviction(S_TRUNK_C,   c)      // acquire for write
    eviction(S_TRUNK_CD,  c)      // dirty release then reacquire
  }

  when ((!s_writeback && no_wait) && io.schedule.ready) {
    transition(S_INVALID,  S_BRANCH,   b && m) // only MMIO can bring us to BRANCH state
    transition(S_INVALID,  S_BRANCH_C, b && c) // C state is only possible if there are inner caches
    transition(S_INVALID,  S_TIP,      m)      // MMIO read
    transition(S_INVALID,  S_TIP_C,    false)  // we would go S_TRUNK_C instead
    transition(S_INVALID,  S_TIP_CD,   false)  // acquire does not cause dirty immediately
    transition(S_INVALID,  S_TIP_D,    m)      // MMIO write
    transition(S_INVALID,  S_TRUNK_C,  c)      // acquire
    transition(S_INVALID,  S_TRUNK_CD, false)  // acquire does not cause dirty immediately

    transition(S_BRANCH,   S_INVALID,  b && p) // probe can do this (flushes run as evictions)
    transition(S_BRANCH,   S_BRANCH_C, b && c) // acquire
    transition(S_BRANCH,   S_TIP,      b && m) // prefetch write
    transition(S_BRANCH,   S_TIP_C,    false)  // we would go S_TRUNK_C instead
    transition(S_BRANCH,   S_TIP_CD,   false)  // acquire does not cause dirty immediately
    transition(S_BRANCH,   S_TIP_D,    b && m) // MMIO write
    transition(S_BRANCH,   S_TRUNK_C,  b && c) // acquire
    transition(S_BRANCH,   S_TRUNK_CD, false)  // acquire does not cause dirty immediately

    transition(S_BRANCH_C, S_INVALID,  b && c && p)
    transition(S_BRANCH_C, S_BRANCH,   b && c)      // clean release (optional)
    transition(S_BRANCH_C, S_TIP,      b && c && m) // prefetch write
    transition(S_BRANCH_C, S_TIP_C,    false)       // we would go S_TRUNK_C instead
    transition(S_BRANCH_C, S_TIP_D,    b && c && m) // MMIO write
    transition(S_BRANCH_C, S_TIP_CD,   false)       // going dirty means we must shoot down clients
    transition(S_BRANCH_C, S_TRUNK_C,  b && c)      // acquire
    transition(S_BRANCH_C, S_TRUNK_CD, false)       // acquire does not cause dirty immediately

    transition(S_TIP,      S_INVALID,  p)
    transition(S_TIP,      S_BRANCH,   p)      // losing TIP only possible via probe
    transition(S_TIP,      S_BRANCH_C, false)  // we would go S_TRUNK_C instead
    transition(S_TIP,      S_TIP_C,    false)  // we would go S_TRUNK_C instead
    transition(S_TIP,      S_TIP_D,    m)      // direct dirty only via MMIO write
    transition(S_TIP,      S_TIP_CD,   false)  // acquire does not make us dirty immediately
    transition(S_TIP,      S_TRUNK_C,  c)      // acquire
    transition(S_TIP,      S_TRUNK_CD, false)  // acquire does not make us dirty immediately

    transition(S_TIP_C,    S_INVALID,  c && p)
    transition(S_TIP_C,    S_BRANCH,   c && p) // losing TIP only possible via probe
    transition(S_TIP_C,    S_BRANCH_C, c && p) // losing TIP only possible via probe
    transition(S_TIP_C,    S_TIP,      c)      // probed while MMIO read || clean release (optional)
    transition(S_TIP_C,    S_TIP_D,    c && m) // direct dirty only via MMIO write
    transition(S_TIP_C,    S_TIP_CD,   false)  // going dirty means we must shoot down clients
    transition(S_TIP_C,    S_TRUNK_C,  c)      // acquire
    transition(S_TIP_C,    S_TRUNK_CD, false)  // acquire does not make us immediately dirty

    transition(S_TIP_D,    S_INVALID,  p)
    transition(S_TIP_D,    S_BRANCH,   p)      // losing D is only possible via probe
    transition(S_TIP_D,    S_BRANCH_C, p && c) // probed while acquire shared
    transition(S_TIP_D,    S_TIP,      p)      // probed while MMIO read || outer probe.toT (optional)
    transition(S_TIP_D,    S_TIP_C,    false)  // we would go S_TRUNK_C instead
    transition(S_TIP_D,    S_TIP_CD,   false)  // we would go S_TRUNK_CD instead
    transition(S_TIP_D,    S_TRUNK_C,  p && c) // probed while acquired
    transition(S_TIP_D,    S_TRUNK_CD, c)      // acquire

    transition(S_TIP_CD,   S_INVALID,  c && p)
    transition(S_TIP_CD,   S_BRANCH,   c && p) // losing D is only possible via probe
    transition(S_TIP_CD,   S_BRANCH_C, c && p) // losing D is only possible via probe
    transition(S_TIP_CD,   S_TIP,      c && p) // probed while MMIO read || outer probe.toT (optional)
    transition(S_TIP_CD,   S_TIP_C,    false)  // we would go S_TRUNK_C instead
    transition(S_TIP_CD,   S_TIP_D,    c)      // MMIO write || clean release (optional)
    transition(S_TIP_CD,   S_TRUNK_C,  c && p) // probed while acquire
    transition(S_TIP_CD,   S_TRUNK_CD, c)      // acquire

    transition(S_TRUNK_C,  S_INVALID,  c && p)
    transition(S_TRUNK_C,  S_BRANCH,   c && p) // losing TIP only possible via probe
    transition(S_TRUNK_C,  S_BRANCH_C, c && p) // losing TIP only possible via probe
    transition(S_TRUNK_C,  S_TIP,      c)      // MMIO read || clean release (optional)
    transition(S_TRUNK_C,  S_TIP_C,    c)      // bounce shared
    transition(S_TRUNK_C,  S_TIP_D,    c)      // dirty release
    transition(S_TRUNK_C,  S_TIP_CD,   c)      // dirty bounce shared
    transition(S_TRUNK_C,  S_TRUNK_CD, c)      // dirty bounce

    transition(S_TRUNK_CD, S_INVALID,  c && p)
    transition(S_TRUNK_CD, S_BRANCH,   c && p) // losing D only possible via probe
    transition(S_TRUNK_CD, S_BRANCH_C, c && p) // losing D only possible via probe
    transition(S_TRUNK_CD, S_TIP,      c && p) // probed while MMIO read || outer probe.toT (optional)
    transition(S_TRUNK_CD, S_TIP_C,    false)  // we would go S_TRUNK_C instead
    transition(S_TRUNK_CD, S_TIP_D,    c)      // dirty release
    transition(S_TRUNK_CD, S_TIP_CD,   c)      // bounce shared
    transition(S_TRUNK_CD, S_TRUNK_C,  c && p) // probed while acquire
  }

  /* Handle response messages from C channel of client. */
  /** one-hot signal to indicate which client is probed by this Probe. */
  val probe_bit = params.clientBit(io.sinkc.bits.source)
  /** This probe is the last probe of the request. */
  val last_probe = (probes_done | probe_bit) === (meta.clients & ~excluded_client)
  /** the client is probed toN. */
  val probe_toN = isToN(io.sinkc.bits.param)
  if (!params.firstLevel) when (io.sinkc.valid) {
    params.ccover( probe_toN && io.schedule.bits.b.bits.param === toB, "MSHR_PROBE_FULL", "Client downgraded to N when asked only to do B")
    params.ccover(!probe_toN && io.schedule.bits.b.bits.param === toB, "MSHR_PROBE_HALF", "Client downgraded to B when asked only to do B")
    // Caution: the probe matches us only in set.
    // We would never allow an outer probe to nest until both w_[rp]probeack complete, so
    // it is safe to just unguardedly update the probe FSM.

    /* clients which are already probed sucessfully. */
    probes_done := probes_done | probe_bit
    /* clients which are probed toN. */
    probes_toN := probes_toN | Mux(probe_toN, probe_bit, UInt(0))
    /* @todo remove this. */
    probes_noT := probes_noT || io.sinkc.bits.param =/= TtoT
    /* update the status register. */
    w_rprobeackfirst := w_rprobeackfirst || last_probe
    w_rprobeacklast := w_rprobeacklast || (last_probe && io.sinkc.bits.last)
    w_pprobeackfirst := w_pprobeackfirst || last_probe
    w_pprobeacklast := w_pprobeacklast || (last_probe && io.sinkc.bits.last)
    /** Last probe has come?
      *
      * @todo. request.offset === UInt(0) and what is wormhole.
      *        Allow wormhole routing from sinkC if the first request beat has offset 0
      */
    val set_pprobeack = last_probe && (io.sinkc.bits.last || request.offset === UInt(0))
    w_pprobeack := w_pprobeack || set_pprobeack
    params.ccover(!set_pprobeack && w_rprobeackfirst, "MSHR_PROBE_SERIAL", "Sequential routing of probe response data")
    params.ccover( set_pprobeack && w_rprobeackfirst, "MSHR_PROBE_WORMHOLE", "Wormhole routing of probe response data")
    /** If ProbeAckData, convert to dirty.
      * However, meta-data updates need to be done more cautiously
      * */
    when (meta.state =/= INVALID && io.sinkc.bits.tag === meta.tag && io.sinkc.bits.data) { meta.dirty := Bool(true) } // !!!
  }
  /* D Channel. */
  when (io.sinkd.valid) {
    /* process Grant and GrantData. */
    when (io.sinkd.bits.opcode === Grant || io.sinkd.bits.opcode === GrantData) {
      /* store a sink register, let E to match. */
      sink := io.sinkd.bits.sink
      w_grantfirst := Bool(true)
      w_grantlast := io.sinkd.bits.last
      /* Record if we need to prevent taking ownership. */
      bad_grant := io.sinkd.bits.denied
      /* @todo Allow wormhole routing for requests whose first beat has offset 0. */
      w_grant := request.offset === UInt(0) || io.sinkd.bits.last
      params.ccover(io.sinkd.bits.opcode === GrantData && request.offset === UInt(0), "MSHR_GRANT_WORMHOLE", "Wormhole routing of grant response data")
      params.ccover(io.sinkd.bits.opcode === GrantData && request.offset =/= UInt(0), "MSHR_GRANT_SERIAL", "Sequential routing of grant response data")
      gotT := io.sinkd.bits.param === toT
    }
      /* Directory has finished. */
    .elsewhen (io.sinkd.bits.opcode === ReleaseAck) {
      w_releaseack := Bool(true)
    }
  }
  /* got reply in E channel. */
  when (io.sinke.valid) {
    w_grantack := Bool(true)
  }

  // Bootstrap new requests
  val allocate_as_full = Wire(new FullRequest(params), init = io.allocate.bits)
  /** metadata assign to [[meta]], if tag match, reuse metadata lasttime, or query from directory. */
  val new_meta = Mux(io.allocate.valid && io.allocate.bits.repeat, final_meta_writeback, io.directory.bits)
  /* grantee don't waste first cycle when new request comes. */
  val new_request = Mux(io.allocate.valid, allocate_as_full, request)
  val new_needT = needT(new_request.opcode, new_request.param)
  val new_clientBit = params.clientBit(new_request.source)
  /** Don't probe client of this request itself. */
  val new_skipProbe = Mux(skipProbeN(new_request.opcode), new_clientBit, UInt(0))

  val prior = cacheState(final_meta_writeback, Bool(true))
  def bypass(from: CacheState, cover: Boolean)(implicit sourceInfo: SourceInfo) {
    if (cover) {
      params.ccover(prior === from.code, s"MSHR_${from}_BYPASS", s"State bypass transition from ${from} ${cfg}")
    } else {
      assert(!(prior === from.code), s"State bypass from ${from} should be impossible ${cfg}")
    }
  }

  when (io.allocate.valid && io.allocate.bits.repeat) {
    bypass(S_INVALID,   f || p) // Can lose permissions (probe/flush)
    bypass(S_BRANCH,    b)      // MMIO read to read-only device
    bypass(S_BRANCH_C,  b && c) // you need children to become C
    bypass(S_TIP,       true)   // MMIO read || clean release can lead to this state
    bypass(S_TIP_C,     c)      // needs two clients || client + mmio || downgrading client
    bypass(S_TIP_CD,    c)      // needs two clients || client + mmio || downgrading client
    bypass(S_TIP_D,     true)   // MMIO write || dirty release lead here
    bypass(S_TRUNK_C,   c)      // acquire for write
    bypass(S_TRUNK_CD,  c)      // dirty release then reacquire
  }

  /* allocate new MSHR. */
  when (io.allocate.valid) {
    assert (!request_valid || (no_wait && io.schedule.fire()))
    request_valid := Bool(true)
    request := io.allocate.bits
  }

  /** When directory of this MSHR is valid start to schedule. */
  when (io.directory.valid || (io.allocate.valid && io.allocate.bits.repeat)) {
    meta_valid := Bool(true)
    meta := new_meta
    probes_done := UInt(0)
    probes_toN := UInt(0)
    probes_noT := Bool(false)
    gotT := Bool(false)
    bad_grant := Bool(false)

    // These should already be either true or turning true
    // We clear them here explicitly to simplify the mux tree
    s_rprobe         := Bool(true)
    w_rprobeackfirst := Bool(true)
    w_rprobeacklast  := Bool(true)
    s_release        := Bool(true)
    w_releaseack     := Bool(true)
    s_pprobe         := Bool(true)
    s_acquire        := Bool(true)
    s_flush          := Bool(true)
    w_grantfirst     := Bool(true)
    w_grantlast      := Bool(true)
    w_grant          := Bool(true)
    w_pprobeackfirst := Bool(true)
    w_pprobeacklast  := Bool(true)
    w_pprobeack      := Bool(true)
    s_probeack       := Bool(true)
    s_grantack       := Bool(true)
    s_execute        := Bool(true)
    w_grantack       := Bool(true)
    s_writeback      := Bool(true)

    // For C channel requests (ie: Release[Data])
    when (new_request.prio(2) && Bool(!params.firstLevel)) {
      /* indicate this MSHR is in executing. */
      s_execute := Bool(false)
      /* if ReleaseData and original is not dirty, need to write metadata, cacheline is dirty. */
      when (new_request.opcode(0) && !new_meta.dirty) {
        /* directory writeback is scheduled. */
        s_writeback := Bool(false)
      }
      /* request permission is released to Branch, if this is a TRUNK, need write metadata this cacheline transform to TIP */
      when (isToB(new_request.param) && new_meta.state === TRUNK) {
        /* directory writeback is scheduled. */
        s_writeback := Bool(false)
      }
      /* request permission is released to None, and any of clients is invalided. */
      when (isToN(new_request.param) && (new_meta.clients & new_clientBit) =/= UInt(0)) {
        /* directory writeback is scheduled. */
        s_writeback := Bool(false)
      }
      /* release need hit. */
      assert (new_meta.hit)
    }
    // For X channel requests (ie: flush)
    .elsewhen (new_request.control && Bool(params.control)) { // new_request.prio(0)
      /** indicate this MSHR is flush cacheline. */
      s_flush := Bool(false)
      /* if not hit, don't need to do anything.
       * if hit, scheduler something.
       */
      when (new_meta.hit) {
        /* Release is scheduled. */
        s_release := Bool(false)
        /* wait for ReleaseAck. */
        w_releaseack := Bool(false)
        /* if there are caches in clients, probe them. */
        when (Bool(!params.firstLevel) && (new_meta.clients =/= UInt(0))) {
          /* Probe caused by Release is scheduled. */
          s_rprobe := Bool(false)
          /* wait for ProbeAck. */
          w_rprobeackfirst := Bool(false)
          w_rprobeacklast := Bool(false)
        }
      }
    }
    // For A channel requests
    .otherwise {
      /** indicate this MSHR is in executing. */
      s_execute := Bool(false)
      /* If not hit, directory will return a victim cacheline to store new data.
       * if this cacheline is invalid, don't need to do a eviction otherwise evict the cacheline.
       */
      when (!new_meta.hit && new_meta.state =/= INVALID) {
        /* release data is scheduled. */
        s_release := Bool(false)
        /* wait for ReleaseAck. */
        w_releaseack := Bool(false)
        /* if there are clients containing this cacheline. Probe it back firstly. */
        when (Bool(!params.firstLevel) & (new_meta.clients =/= UInt(0))) {
          /* Probe caused by Release is scheduled. */
          s_rprobe := Bool(false)
          /* Wait for ProbeAck caused by Release. */
          w_rprobeackfirst := Bool(false)
          w_rprobeacklast := Bool(false)
        }
      }
      /* 1. not hit, acquire data from manger.
       * 2. cache is BRANCH and need to request need TIP permission.
       *
       * @todo since this is last level cache, remove CacheCork and always give T from manger.
       */
      when (!new_meta.hit || (new_meta.state === BRANCH && new_needT)) {
        /* acquire data is scheduled. */
        s_acquire := Bool(false)
        /* wait for Grant or GrantData */
        w_grantfirst := Bool(false)
        w_grantlast := Bool(false)
        w_grant := Bool(false)
        /* GrantAck is scheduled. */
        s_grantack := Bool(false)
        /* Directory is scheduled. */
        s_writeback := Bool(false)
      }
      /* Schedule Probe caused by permission transform(Acquire).
       *
       * 1. hit
       * 2. [[request]] need TIP permission or this level is TRUNK.
       * 3. There are other clients need to Probe.
       */
      when (Bool(!params.firstLevel) && (new_meta.hit &&
            (new_needT || new_meta.state === TRUNK) &&
            (new_meta.clients & ~new_skipProbe) =/= UInt(0))) {
        /* a Probe caused by Acquire is scheduled. */
        s_pprobe := Bool(false)
        /* wait for ProbeAck. */
        w_pprobeackfirst := Bool(false)
        w_pprobeacklast := Bool(false)
        w_pprobeack := Bool(false)
        /* directory writeback is scheduled. */
        s_writeback := Bool(false)
      }
      /* Scheduler needed by Acquire. */
      when (new_request.opcode === AcquireBlock || new_request.opcode === AcquirePerm) {
        /* wait for GrantAck. */
        w_grantack := Bool(false)
        /* directory writeback is scheduled. */
        s_writeback := Bool(false)
      }
      /* PutFullData
       * PutPartialData
       * ArithmeticLogic
       * LogicalData
       *
       * write will cause this Tip dirty.
       */
      when (!new_request.opcode(2) && new_meta.hit && !new_meta.dirty) {
        s_writeback := Bool(false)
      }
    }
  }
}
