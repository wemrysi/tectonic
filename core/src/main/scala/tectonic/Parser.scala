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

/*
 * This file substantially copied from the Jawn project and lightly modified. All
 * credit (and much love and thanks!) to Erik Osheim and the other Jawn authors.
 * All copied lines remain under original copyright and license.
 *
 * Copyright Erik Osheim, 2012-2018
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

import scala.{inline, sys, Array, Boolean, Char, Int, Nothing, Predef, Unit}, Predef._
import scala.annotation.{switch, tailrec}

import java.lang.{CharSequence, Exception, IndexOutOfBoundsException, String}
import java.nio.charset.Charset

case class ParseException(msg: String, index: Int, line: Int, col: Int) extends Exception(msg)

case class IncompleteParseException(msg: String) extends Exception(msg)

/**
 * Parser implements a state machine for correctly parsing JSON data.
 *
 * The trait relies on a small number of methods which are left
 * abstract, and which generalize parsing based on whether the input
 * is in Bytes or Chars, coming from Strings, files, or other input.
 * All methods provided here are protected, so different parsers can
 * choose which functionality to expose.
 *
 * Parser is parameterized on J, which is the type of the JSON AST it
 * will return. Jawn can produce any AST for which a Facade[J] is
 * available.
 *
 * The parser trait does not hold any state itself, but particular
 * implementations will usually hold state. Parser instances should
 * not be reused between parsing runs.
 *
 * For now the parser requires input to be in UTF-8. This requirement
 * may eventually be relaxed.
 */
abstract class Parser[A](plate: Plate[A]) {

  protected[this] final val utf8 = Charset.forName("UTF-8")

  /**
   * Read the byte/char at 'i' as a Char.
   *
   * Note that this should not be used on potential multi-byte
   * sequences.
   */
  protected[this] def at(i: Int): Char

  /**
   * Read the bytes/chars from 'i' until 'j' as a String.
   */
  protected[this] def at(i: Int, j: Int): CharSequence

  /**
   * Return true iff 'i' is at or beyond the end of the input (EOF).
   */
  protected[this] def atEof(i: Int): Boolean

  /**
   * The reset() method is used to signal that we're working from the
   * given position, and any previous data can be released. Some
   * parsers (e.g.  StringParser) will ignore release, while others
   * (e.g. PathParser) will need to use this information to release
   * and allocate different areas.
   */
  protected[this] def reset(i: Int): Int

  /**
   * The checkpoint() method is used to allow some parsers to store
   * their progress.
   */
  protected[this] def checkpoint(state: Int, i: Int): Unit

  /**
   * Should be called when parsing is finished.
   */
  protected[this] def close(): Unit

  /**
   * Valid parser states.
   */
  @inline protected[this] final val ARRBEG = 6
  @inline protected[this] final val OBJBEG = 7
  @inline protected[this] final val DATA = 1
  @inline protected[this] final val KEY = 2
  @inline protected[this] final val SEP = 3
  @inline protected[this] final val ARREND = 4
  @inline protected[this] final val OBJEND = 5

  // constants inlined here to avoid memory barriers on access
  private[this] val EncMap = Enclosure.Map
  private[this] val EncArray = Enclosure.Array
  private[this] val EncMeta = Enclosure.Meta
  private[this] val EncNone = Enclosure.None

  protected[this] def newline(i: Int): Unit
  protected[this] def line(): Int
  protected[this] def column(i: Int): Int

  protected[this] final val HexChars: Array[Int] = {
    val arr = new Array[Int](128)
    var i = 0
    while (i < 10) { arr(i + '0') = i; i += 1 }
    i = 0
    while (i < 16) { arr(i + 'a') = 10 + i; arr(i + 'A') = 10 + i; i += 1 }
    arr
  }

  /**
   * Used to generate error messages with character info and offsets.
   */
  protected[this] def die(i: Int, msg: String): Nothing = {
    val y = line() + 1
    val x = column(i) + 1
    val s = "%s got %s (line %d, column %d)" format (msg, at(i), y, x)
    throw ParseException(s, i, y, x)
  }

  /**
   * Used to generate messages for internal errors.
   *
   * This should only be used in situations where a possible bug in
   * the parser was detected. For errors in user-provided JSON, use
   * die().
   */
  protected[this] def error(msg: String) =
    sys.error(msg)

  /**
   * Parse the given number, and add it to the given context.
   *
   * We don't actually instantiate a number here, but rather pass the
   * string of for future use. Facades can choose to be lazy and just
   * store the string. This ends up being way faster and has the nice
   * side-effect that we know exactly how the user represented the
   * number.
   */
  protected[this] final def parseNum(i: Int): Int = {
    var j = i
    var c = at(j)
    var decIndex = -1
    var expIndex = -1

    if (c == '-') {
      j += 1
      c = at(j)
    }
    if (c == '0') {
      j += 1
      c = at(j)
    } else if ('1' <= c && c <= '9') {
      while ('0' <= c && c <= '9') { j += 1; c = at(j) }
    } else {
      die(i, "expected digit")
    }

    if (c == '.') {
      decIndex = j - i
      j += 1
      c = at(j)
      if ('0' <= c && c <= '9') {
        while ('0' <= c && c <= '9') { j += 1; c = at(j) }
      } else {
        die(i, "expected digit")
      }
    }

    if (c == 'e' || c == 'E') {
      expIndex = j - i
      j += 1
      c = at(j)
      if (c == '+' || c == '-') {
        j += 1
        c = at(j)
      }
      if ('0' <= c && c <= '9') {
        while ('0' <= c && c <= '9') { j += 1; c = at(j) }
      } else {
        die(i, "expected digit")
      }
    }

    plate.num(at(i, j), decIndex, expIndex)
    j
  }

  /**
   * Parse the given number, and add it to the given context.
   *
   * This method is a bit slower than parseNum() because it has to be
   * sure it doesn't run off the end of the input.
   *
   * Normally (when operating in rparse in the context of an outer
   * array or object) we don't need to worry about this and can just
   * grab characters, because if we run out of characters that would
   * indicate bad input. This is for cases where the number could
   * possibly be followed by a valid EOF.
   *
   * This method has all the same caveats as the previous method.
   */
  protected[this] final def parseNumSlow(i: Int): Int = {
    var j = i
    var c = at(j)
    var decIndex = -1
    var expIndex = -1

    if (c == '-') {
      // any valid input will require at least one digit after -
      j += 1
      c = at(j)
    }
    if (c == '0') {
      j += 1
      if (atEof(j)) {
        plate.num(at(i, j), decIndex, expIndex)
        return j
      }
      c = at(j)
    } else if ('1' <= c && c <= '9') {
      while ('0' <= c && c <= '9') {
        j += 1
        if (atEof(j)) {
          plate.num(at(i, j), decIndex, expIndex)
          return j
        }
        c = at(j)
      }
    } else {
      die(i, "expected digit")
    }

    if (c == '.') {
      // any valid input will require at least one digit after .
      decIndex = j - i
      j += 1
      c = at(j)
      if ('0' <= c && c <= '9') {
        while ('0' <= c && c <= '9') {
          j += 1
          if (atEof(j)) {
            plate.num(at(i, j), decIndex, expIndex)
            return j
          }
          c = at(j)
        }
      } else {
        die(i, "expected digit")
      }
    }

    if (c == 'e' || c == 'E') {
      // any valid input will require at least one digit after e, e+, etc
      expIndex = j - i
      j += 1
      c = at(j)
      if (c == '+' || c == '-') {
        j += 1
        c = at(j)
      }
      if ('0' <= c && c <= '9') {
        while ('0' <= c && c <= '9') {
          j += 1
          if (atEof(j)) {
            plate.num(at(i, j), decIndex, expIndex)
            return j
          }
          c = at(j)
        }
      } else {
        die(i, "expected digit")
      }
    }

    plate.num(at(i, j), decIndex, expIndex)
    j
  }

  /**
   * Generate a Char from the hex digits of "\u1234" (i.e. "1234").
   *
   * NOTE: This is only capable of generating characters from the basic plane.
   * This is why it can only return Char instead of Int.
   */
  protected[this] final def descape(s: CharSequence): Char = {
    val hc = HexChars
    var i = 0
    var x = 0
    while (i < 4) {
      x = (x << 4) | hc(s.charAt(i).toInt)
      i += 1
    }
    x.toChar
  }

  /**
   * Parse the JSON string starting at 'i' and save it into the plate.
   * If key is true, save the string with 'nestMap', otherwise use 'str'.
   */
  protected[this] def parseString(i: Int, key: Boolean): Int

  /**
   * Parse the JSON constant "true".
   *
   * Note that this method assumes that the first character has already been checked.
   */
  protected[this] final def parseTrue(i: Int): Unit =
    if (at(i + 1) == 'r' && at(i + 2) == 'u' && at(i + 3) == 'e') {
      plate.tru()
    } else {
      die(i, "expected true")
    }

  /**
   * Parse the JSON constant "false".
   *
   * Note that this method assumes that the first character has already been checked.
   */
  protected[this] final def parseFalse(i: Int): Unit =
    if (at(i + 1) == 'a' && at(i + 2) == 'l' && at(i + 3) == 's' && at(i + 4) == 'e') {
      plate.fls()
    } else {
      die(i, "expected false")
    }

  /**
   * Parse the JSON constant "null".
   *
   * Note that this method assumes that the first character has already been checked.
   */
  protected[this] final def parseNull(i: Int): Unit =
    if (at(i + 1) == 'u' && at(i + 2) == 'l' && at(i + 3) == 'l') {
      plate.nul()
    } else {
      die(i, "expected null")
    }

  /**
   * Parse and return the next JSON value and the position beyond it.
   */
  protected[this] final def parse(i: Int): Int = try {
    (at(i): @switch) match {
      // ignore whitespace
      case ' ' => parse(i + 1)
      case '\t' => parse(i + 1)
      case '\r' => parse(i + 1)
      case '\n' => newline(i); parse(i + 1)

      // if we have a recursive top-level structure, we'll delegate the parsing
      // duties to our good friend rparse().
      case '[' => rparse(ARRBEG, i + 1)
      case '{' => rparse(OBJBEG, i + 1)

      // we have a single top-level number
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        val j = parseNumSlow(i)
        plate.finishRow()
        j

      // we have a single top-level string
      case '"' =>
        val j = parseString(i, false)
        plate.finishRow()
        j

      // we have a single top-level constant
      case 't' =>
        parseTrue(i)
        plate.finishRow()
        i + 4

      case 'f' =>
        parseFalse(i)
        plate.finishRow()
        i + 5

      case 'n' =>
        parseNull(i)
        plate.finishRow()
        i + 4

      // invalid
      case _ => die(i, "expected json value")
    }
  } catch {
    case _: IndexOutOfBoundsException =>
      throw IncompleteParseException("exhausted input")
  }

  /**
   * Tail-recursive parsing method to do the bulk of JSON parsing.
   *
   * This single method manages parser states, data, etc. Except for
   * parsing non-recursive values (like strings, numbers, and
   * constants) all important work happens in this loop (or in methods
   * it calls, like reset()).
   *
   * Currently the code is optimized to make use of switch
   * statements. Future work should consider whether this is better or
   * worse than manually constructed if/else statements or something
   * else. Also, it may be possible to reorder some cases for speed
   * improvements.
   */
  @tailrec
  protected[this] final def rparse(state: Int, j: Int): Int = {
    val i = reset(j)
    checkpoint(state, i)

    val c = at(i)

    if (c == '\n') {
      newline(i)
      rparse(state, i + 1)
    } else if (c == ' ' || c == '\t' || c == '\r') {
      rparse(state, i + 1)
    } else if (state == DATA) {
      // we are inside an object or array expecting to see data
      if (c == '[') {
        rparse(ARRBEG, i + 1)
      } else if (c == '{') {
        rparse(OBJBEG, i + 1)
      } else {
        if ((c >= '0' && c <= '9') || c == '-') {
          val j = parseNum(i)
          rparse(if (plate.enclosure() eq EncMap) OBJEND else ARREND, j)
        } else if (c == '"') {
          val j = parseString(i, false)
          rparse(if (plate.enclosure() eq EncMap) OBJEND else ARREND, j)
        } else if (c == 't') {
          parseTrue(i)
          rparse(if (plate.enclosure() eq EncMap) OBJEND else ARREND, i + 4)
        } else if (c == 'f') {
          parseFalse(i)
          rparse(if (plate.enclosure() eq EncMap) OBJEND else ARREND, i + 5)
        } else if (c == 'n') {
          parseNull(i)
          rparse(if (plate.enclosure() eq EncMap) OBJEND else ARREND, i + 4)
        } else {
          die(i, "expected json value")
        }
      }
    } else if (
      (c == ']' && (state == ARREND || state == ARRBEG)) ||
      (c == '}' && (state == OBJEND || state == OBJBEG))
    ) {
      // we are inside an array or object and have seen a key or a closing
      // brace, respectively.
      if (plate.enclosure() eq EncNone) {    // TODO remove this check for performance
        error("invalid enclosure: " + plate.enclosure())
      } else {
        (state: @switch) match {
          case ARREND => plate.unnestArr()
          case OBJEND => plate.unnestMap()
          case ARRBEG => plate.emptyArr()
          case OBJBEG => plate.emptyMap()
        }

        val enc = plate.enclosure()

        if (enc eq EncNone) {
          plate.finishRow()
          i + 1
        } else {
          rparse(if (enc eq EncMap) OBJEND else ARREND, i + 1)
        }
      }
    } else if (state == KEY) {
      // we are in an object expecting to see a key.
      if (c == '"') {
        rparse(SEP, parseString(i, true))
      } else {
        die(i, "expected \"")
      }
    } else if (state == SEP) {
      // we are in an object just after a key, expecting to see a colon.
      if (c == ':') {
        rparse(DATA, i + 1)
      } else {
        die(i, "expected :")
      }
    } else if (state == ARREND) {
      // we are in an array, expecting to see a comma (before more data).
      if (c == ',') {
        rparse(DATA, i + 1)
      } else {
        die(i, "expected ] or ,")
      }
    } else if (state == OBJEND) {
      // we are in an object, expecting to see a comma (before more data).
      if (c == ',') {
        rparse(KEY, i + 1)
      } else {
        die(i, "expected } or ,")
      }
    } else if (state == ARRBEG) {
      // we are starting an array, expecting to see data or a closing bracket.
      rparse(DATA, i)
    } else {
      // we are starting an object, expecting to see a key or a closing brace.
      rparse(KEY, i)
    }
  }
}
