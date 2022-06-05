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

/** Interface between MSHR and Source X.
  * ACK to flush controller.
  */
class SourceXRequest(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val fail = Bool()
}

class SourceX(params: InclusiveCacheParameters) extends Module
{
  val io = new Bundle {
    /** Request from MSHR. */
    val req = Decoupled(new SourceXRequest(params)).flip
    /** Wired to flush controller. */
    val x = Decoupled(new SourceXRequest(params))
  }

  val x = Wire(io.x) // ready must not depend on valid
  /* construct a buffer of X. */
  io.x <> Queue(x, 1)

  /* couple controller ready directly to MSHR. */
  io.req.ready := x.ready
  /* couple MSHR valid directly to controller. */
  x.valid := io.req.valid
  params.ccover(x.valid && !x.ready, "SOURCEX_STALL", "Backpressure when sending a control message")

  x.bits := io.req.bits
}
