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
import freechips.rocketchip.util._

/**
  * @param gen Type of entry
  * @param queues size of queues
  * @param entries size of entries
  * @param bypass if ture it will bypass
  *
  */
case class ListBufferParameters[T <: Data](gen: T, queues: Int, entries: Int, bypass: Boolean)
{
  val queueBits = log2Up(queues)
  val entryBits = log2Up(entries)
}

class ListBufferPush[T <: Data](params: ListBufferParameters[T]) extends GenericParameterizedBundle(params)
{
  /** queue index to push. */
  val index = UInt(width = params.queueBits)
  /** enqueue data. */
  val data  = params.gen.asOutput
}

/** Multiple linked list sharing a block of memory.
  *
  * IO
  * push: Push data with bits and data.
  * valid: queue has data.
  * pop: request correspond queue to pop data.
  * data: popped data.
  */
class ListBuffer[T <: Data](params: ListBufferParameters[T]) extends Module
{
  val io = new Bundle {
    /** push data to correspond queues identified by [[ListBufferPush.index]].
      * push is visible on the same cycle; flow queues.
      */
    val push  = Decoupled(new ListBufferPush(params)).flip
    /** bits indicate queues is valid. */
    val valid = UInt(width = params.queues)
    /** request correspond queue to pop data. */
    val pop   = Valid(UInt(width = params.queueBits)).flip
    /** popped data. */
    val data  = params.gen.asOutput
  }

  /** indicate a queue is not empty. */
  val valid = RegInit(UInt(0, width=params.queues))
  /** head pointer of each queue. */
  val head  = Mem(params.queues, UInt(width = params.entryBits))
  /** tail pointer of each queue. */
  val tail  = Mem(params.queues, UInt(width = params.entryBits))

  /** indicate this entry has a valid data. */
  val used  = RegInit(UInt(0, width=params.entries))
  /** next pointer of a data. */
  val next  = Mem(params.entries, UInt(width = params.entryBits))
  /** entry of data. */
  val data  = Mem(params.entries, params.gen)

  /** find a one-hot index of the first(right first) empty entries. */
  val freeOH = ~(leftOR(~used) << 1) & ~used
  /** index of [[freeOH]]. */
  val freeIdx = OHToUInt(freeOH)

  /** One-hot signal to indicate a queue is pushed in this cycle. */
  val valid_set = Wire(init = UInt(0, width=params.queues))
  /** One-hot signal to indicate a queue is popped to empty in this cycle. */
  val valid_clr = Wire(init = UInt(0, width=params.queues))
  /** One-hot signal to indicate a entries is pushed in this cycle. */
  val used_set  = Wire(init = UInt(0, width=params.entries))
  /** One-hot signal to indicate a entries is popped in this cycle. */
  val used_clr  = Wire(init = UInt(0, width=params.entries))

  /** tail of queue being pushed. */
  val push_tail = tail.read(io.push.bits.index)
  /** singal to indicate is this queue empty before this push. */
  val push_valid = valid(io.push.bits.index)

  /* only allow push when entries are not full */
  io.push.ready := !used.andR()
  /* push logic. */
  when (io.push.fire()) {
    valid_set := UIntToOH(io.push.bits.index, params.queues)
    used_set := freeOH
    data.write(freeIdx, io.push.bits.data)
    /* if queue exist, update [[next]], else create a new queue. */
    when (push_valid) {
      next.write(push_tail, freeIdx)
    } .otherwise {
      head.write(io.push.bits.index, freeIdx)
    }
    tail.write(io.push.bits.index, freeIdx)
  }

  /* Pop logic */

  /** head index of a queue to be popped. */
  val pop_head = head.read(io.pop.bits)
  /** queue to be popped is not empty. */
  val pop_valid = valid(io.pop.bits)

  /** Assign data to IO: pop from head.
    * Bypass push data to the peek port
    */
  io.data := (if (!params.bypass) data.read(pop_head) else Mux(!pop_valid, io.push.bits.data, data.read(pop_head)))
  io.valid := (if (!params.bypass) valid else (valid | valid_set))

  /* It is an error to pop something that is not valid.
   * Cannot pop a empty queue.
   */
  assert (!io.pop.fire() || (io.valid)(io.pop.bits))

  when (io.pop.fire()) {
    /** index which entry is popping. */
    used_clr := UIntToOH(pop_head, params.entries)
    /** if head equals tail, clear the valid bit of this queue*/
    when (pop_head === tail.read(io.pop.bits)) {
      valid_clr := UIntToOH(io.pop.bits, params.queues)
    }
    /** find the next head of this queue. */
    head.write(io.pop.bits, Mux(io.push.fire() && push_valid && push_tail === pop_head, freeIdx, next.read(pop_head)))
  }

  /* if bypass is not set, update state signal[[used]] and [[valid]] in each signal.
   * if bypass is set, empty bypass changes no state.
   */
  when (Bool(!params.bypass) || !io.pop.valid || pop_valid) {
    used  := (used  & ~used_clr)  | used_set
    valid := (valid & ~valid_clr) | valid_set
  }
}
