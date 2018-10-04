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

import scala.{Array, StringContext}
import scala.util.{Left, Right}

import java.lang.{String, SuppressWarnings}

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
        Unnest,
        NestMap("b"),
        Fls,
        Unnest)
    }

    "parse a map within a map" in {
      """{"a": {"b": null }   }""" must parseRowAs(
        NestMap("a"),
        NestMap("b"),
        Nul,
        Unnest,
        Unnest)
    }

    "parse an array with four values" in {
      """["a", 123, "b", false]""" must parseRowAs(
        NestArr,
        Str("a"),
        Unnest,
        NestArr,
        Num("123", -1, -1),
        Unnest,
        NestArr,
        Str("b"),
        Unnest,
        NestArr,
        Fls,
        Unnest)
    }

    "parse two rows of scalars" in {
      """12 true""" must parseAs(Num("12", -1, -1), FinishRow, Tru, FinishRow)
    }

    "parse two rows of non-scalars" in {
      """{"a": 3.14} {"b": false, "c": "abc"}""" must parseAs(
        NestMap("a"),
        Num("3.14", 1, -1),
        Unnest,
        FinishRow,
        NestMap("b"),
        Fls,
        Unnest,
        NestMap("c"),
        Str("abc"),
        Unnest,
        FinishRow)
    }
  }

  def parseRowAs(expected: Event*) =
    parseAs(expected :+ Event.FinishRow: _*)

  def parseAs(expected: Event*): Matcher[String] = { input: String =>
    val parser = AsyncParser(new ReifiedTerminalPlate, AsyncParser.ValueStream)

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
}
