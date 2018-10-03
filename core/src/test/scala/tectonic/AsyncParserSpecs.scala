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

import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification

import scala.{Array, Int, List, Nil, Product, Serializable, StringContext, Unit}
import scala.collection.mutable
import scala.util.{Left, Right}

import java.lang.{CharSequence, String, SuppressWarnings}

@SuppressWarnings(Array("org.wartremover.warts.Equals"))
object AsyncParserSpecs extends Specification {

  "async line-delimited parsing" should {
    import Event._

    "parse all of the scalars" >> {
      "null" >> {
        "null" must parseRowAs(Nul)
      }

      "false" >> {
        "false" must parseRowAs(Fls)
      }

      "true" >> {
        "true" must parseRowAs(Tru)
      }

      "{}" >> {
        "{}" must parseRowAs(Map)
      }

      "[]" >> {
        "[]" must parseRowAs(Arr)
      }

      "number" >> {
        "integral" >> {
          "42" must parseRowAs(Num("42", -1, -1))
        }

        "decimal" >> {
          "3.1415" must parseRowAs(Num("3.1415", 1, -1))
        }

        "exponential" >> {
          "2.99792458e8" must parseRowAs(Num("2.99792458e8", 1, 10))
        }
      }

      "string" >> {
        """"quick brown fox"""" must parseRowAs(Str("quick brown fox"))
      }
    }

    "parse a map with two keys" in {
      """{"a":123, "b": false}""" must parseRowAs(
        NestMap("a"),
        Num("123", -1, -1),
        UnnestMap,
        NestMap("b"),
        Fls,
        UnnestMap)
    }

    "parse a map within a map" in {
      """{"a": {"b": null }   }""" must parseRowAs(
        NestMap("a"),
        NestMap("b"),
        Nul,
        UnnestMap,
        UnnestMap)
    }

    "parse an array with four values" in {
      """["a", 123, "b", false]""" must parseRowAs(
        NestArr(0),
        Str("a"),
        UnnestArr,
        NestArr(1),
        Num("123", -1, -1),
        UnnestArr,
        NestArr(2),
        Str("b"),
        UnnestArr,
        NestArr(3),
        Fls,
        UnnestArr)
    }

    "parse two rows of scalars" in {
      """12 true""" must parseAs(Num("12", -1, -1), FinishRow, Tru, FinishRow)
    }

    "parse two rows of non-scalars" in {
      """{"a": 3.14} {"b": false, "c": "abc"}""" must parseAs(
        NestMap("a"),
        Num("3.14", 1, -1),
        UnnestMap,
        FinishRow,
        NestMap("b"),
        Fls,
        UnnestMap,
        NestMap("c"),
        Str("abc"),
        UnnestMap,
        FinishRow)
    }
  }

  def parseRowAs(expected: Event*) =
    parseAs(expected :+ Event.FinishRow: _*)

  def parseAs(expected: Event*): Matcher[String] = { input: String =>
    val parser = AsyncParser(new ReifyPlate, AsyncParser.ValueStream)

    (parser.absorb(input), parser.finish()) match {
      case (Right(init), Right(tail)) =>
        val results = init ++ tail
        (results == expected.toList, s"expected $expected and got $results")

      case (Left(err), _) =>
        (false, s"failed to parse with error '${err.getMessage}' at ${err.line}:${err.col} (i=${err.index})")

      case (_, Left(err)) =>
        (false, s"failed to parse with error '${err.getMessage}' at ${err.line}:${err.col} (i=${err.index})")
    }
  }

  class ReifyPlate extends Plate[List[Event]] {
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

    def unnestMap(): Signal = {
      events += UnnestMap
      enclosures = enclosures.tail
      Signal.Continue
    }

    def nestArr(index: Int): Signal = {
      events += NestArr(index)
      enclosures ::= Enclosure.Array
      Signal.Continue
    }

    def unnestArr(): Signal = {
      events += UnnestArr
      enclosures = enclosures.tail
      Signal.Continue
    }

    def nestMeta(pathComponent: CharSequence): Signal = {
      events += NestMeta(pathComponent)
      enclosures ::= Enclosure.Meta
      Signal.Continue
    }

    def unnestMeta(): Signal = {
      events += UnnestMeta
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

  sealed trait Event extends Product with Serializable

  object Event {
    case object Nul extends Event
    case object Fls extends Event
    case object Tru extends Event
    case object Map extends Event
    case object Arr extends Event
    final case class Num(s: CharSequence, decIdx: Int, expIdx: Int) extends Event
    final case class Str(s: CharSequence) extends Event

    final case class NestMap(pathComponent: CharSequence) extends Event
    case object UnnestMap extends Event

    final case class NestArr(index: Int) extends Event
    case object UnnestArr extends Event

    final case class NestMeta(pathComponent: CharSequence) extends Event
    case object UnnestMeta extends Event

    case object FinishRow extends Event
  }
}
