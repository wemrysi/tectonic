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

import scala.{Array, Int, List, Nil, Unit}
import scala.collection.mutable

import java.lang.{CharSequence, SuppressWarnings}

@SuppressWarnings(
  Array(
    "org.wartremover.warts.NonUnitStatements",
    "org.wartremover.warts.Var",
    "org.wartremover.warts.TraversableOps"))
final class ReifiedTerminalPlate extends Plate[List[Event]] {
  import Event._

  private val events = new mutable.ListBuffer[Event]
  private var enclosures: List[Enclosure] = Enclosure.None :: Nil

  def nul(): Signal = {
    events += Nul
    Signal.Continue
  }

  def fls(): Signal = {
    events += Fls
    Signal.Continue
  }

  def tru(): Signal = {
    events += Tru
    Signal.Continue
  }

  def map(): Signal = {
    events += Map
    Signal.Continue
  }

  def arr(): Signal = {
    events += Arr
    Signal.Continue
  }

  def num(s: CharSequence, decIdx: Int, expIdx: Int): Signal = {
    events += Num(s, decIdx, expIdx)
    Signal.Continue
  }

  def str(s: CharSequence): Signal = {
    events += Str(s)
    Signal.Continue
  }

  def enclosure(): Enclosure = enclosures.head

  def nestMap(pathComponent: CharSequence): Signal = {
    events += NestMap(pathComponent)
    enclosures ::= Enclosure.Map
    Signal.Continue
  }

  def nestArr(): Signal = {
    events += NestArr
    enclosures ::= Enclosure.Array
    Signal.Continue
  }

  def nestMeta(pathComponent: CharSequence): Signal = {
    events += NestMeta(pathComponent)
    enclosures ::= Enclosure.Meta
    Signal.Continue
  }

  def unnest(): Signal = {
    events += Unnest
    enclosures = enclosures.tail
    Signal.Continue
  }

  def finishRow(): Unit = events += FinishRow

  def finishBatch(): List[Event] = {
    val back = events.toList
    events.clear()
    back
  }
}
