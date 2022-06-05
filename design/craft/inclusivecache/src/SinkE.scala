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

/** Interface between MSHR and Sink E. */
class SinkEResponse(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val sink = UInt(width = params.inner.bundle.sinkBits)
}

class SinkE(params: InclusiveCacheParameters) extends Module
{
  val io = new Bundle {
    /** Response to MSHR. */
    val resp = Valid(new SinkEResponse(params))
    /** Wired from clients E channel. */
    val e = Decoupled(new TLBundleE(params.inner.bundle)).flip
  }

  /* Inclusive Cache is last level only. */
  if (params.firstLevel) {
    /* Tie off unused ports. */
    io.resp.valid := Bool(false)
    io.e.ready := Bool(true)
  } else {
    /** Based on parameter of micro architecture, construct a buffer of E. */
    val e = params.micro.innerBuf.e(io.e)

    /** Always accept E from clients. */
    e.ready := Bool(true)
    io.resp.valid := e.valid
    io.resp.bits.sink := e.bits.sink
  }
}
