/*
 * Copyright 2014–2018 SlamData Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package tectonic
package json

import cats.effect.{ContextShift, IO}

import _root_.fs2.io.file

import jawnfs2._

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Param, Scope, State}
import org.openjdk.jmh.infra.Blackhole

import tectonic.fs2.StreamParser

import scala.concurrent.ExecutionContext

import java.nio.file.Paths
import java.util.concurrent.{Executors, TimeUnit}

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(Scope.Benchmark)
class ParserBenchmarks {
  val TectonicFramework = "tectonic"
  val JawnFramework = "jawn"

  private[this] implicit val CS: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  private[this] val BlockingEC =
    ExecutionContext.fromExecutor(Executors newCachedThreadPool { r =>
      val t = new Thread(r)
      t.setDaemon(true)
      t
    })

  private[this] val ChunkSize = 65536

  private[this] val ResourceDir =
    Paths.get(System.getProperty("project.resource.dir"))

  private[this] val UghPath = ResourceDir.resolve("ugh10k.json")

  // optimized columnar plate vs optimized row facade (invented out of thin air by Daniel and Alissa 😁)

  val tectonicVectorCost: Long = 4   // Cons object allocation + memory store
  val tectonicTinyScalarCost: Long = 8    // hashmap get + bitset put
  val tectonicScalarCost: Long = 16   // hashmap get + check on array + amortized resize/allocate + array store
  val tectonicRowCost: Long = 2   // increment integer + bounds check + amortized reset
  val tectonicBatchCost: Long = 1   // (maybe) reset state + bounds check

  val numericCost: Long = 512   // scalarCost + crazy numeric shenanigans

  val jawnVectorAddCost: Long = 32   // hashmap something + checks + allocations + stuff
  val jawnVectorFinalCost: Long = 4   // final allocation + memory store
  val jawnScalarCost: Long = 2     // object allocation
  val jawnTinyScalarCost: Long = 1   // non-volatile memory read

  // params

  @Param(Array("tectonic", "jawn"))
  var framework: String = _

  // benchmarks

  // includes the cost of file IO; not sure if that's a good thing?
  @Benchmark
  def consumeUgh(bh: Blackhole): Unit = {
    val plate = new BlackholePlate(
      tectonicVectorCost,
      tectonicScalarCost,
      tectonicTinyScalarCost,
      numericCost,
      tectonicRowCost,
      tectonicBatchCost)

    implicit val facade = new BlackholeFacade(
      jawnVectorAddCost,
      jawnVectorFinalCost,
      jawnScalarCost,
      jawnTinyScalarCost,
      numericCost)

    val contents = file.readAll[IO](UghPath, BlockingEC, ChunkSize)

    val processed = if (framework == TectonicFramework)
      contents.through(StreamParser[IO, Nothing](IO(Parser(plate, Parser.UnwrapArray))))
    else
      contents.chunks.unwrapJsonArray

    processed.compile.drain.unsafeRunSync()
  }
}
