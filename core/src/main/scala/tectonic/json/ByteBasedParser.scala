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

import tectonic.util.CharBuilder

import scala.{Array, Boolean, Byte, Int, StringContext}
import scala.annotation.switch

import java.lang.SuppressWarnings

/**
 * Trait used when the data to be parsed is in UTF-8.
 *
 * This parser has to translate input bytes to Chars and Strings. It
 * provides a byte() method to access individual bytes, and also
 * parser strings from bytes.
 *
 * Its parseString() implementation has two cases. In the first case
 * (the hot path) the string has no escape sequences and we can just
 * UTF-8 decode the entire set of bytes. In the second case, it goes
 * to some trouble to be sure to de-escape correctly given that the
 * input data is UTF-8.
 */
@SuppressWarnings(Array("org.wartremover.warts.Var"))
trait ByteBasedParser[A] extends Parser[A] {
  protected[this] def byte(i: Int): Byte

  /**
   * See if the string has any escape sequences. If not, return the end of the
   * string. If so, bail out and return -1.
   *
   * This method expects the data to be in UTF-8 and accesses it as bytes. Thus
   * we can just ignore any bytes with the highest bit set.
   */
  @SuppressWarnings(
    Array(
      "org.wartremover.warts.Equals",
      "org.wartremover.warts.Return",
      "org.wartremover.warts.While"))
  protected[this] final def parseStringSimple(i: Int): Int = {
    var j = i
    var c: Int = byte(j) & 0xff
    while (c != 34) {
      if (c < 32) die(j, s"control char ($c) in string")
      if (c == 92) return -1
      j += 1
      c = byte(j) & 0xff
    }
    j + 1
  }

  /**
   * Parse the string according to JSON rules, and add to the given context.
   *
   * This method expects the data to be in UTF-8 and accesses it as bytes.
   */
  @SuppressWarnings(
    Array(
      "org.wartremover.warts.Equals",
      "org.wartremover.warts.NonUnitStatements",
      "org.wartremover.warts.While",
      "org.wartremover.warts.Return"))
  protected[this] final def parseString(i: Int, key: Boolean): Int = {
    val k = parseStringSimple(i + 1)
    if (k != -1) {
      val cs = at(i + 1, k - 1)
      if (key) plate.nestMap(cs) else plate.str(cs)
      return k
    }

    // TODO: we might be able to do better by identifying where
    // escapes occur, and then translating the intermediate strings in
    // one go.

    var j = i + 1
    val sb = new CharBuilder

    var c: Int = byte(j) & 0xff
    while (c != 34) { // "
      if (c == 92) { // \
        (byte(j + 1): @switch) match {
          case 98 => { sb.append('\b'); j += 2 }
          case 102 => { sb.append('\f'); j += 2 }
          case 110 => { sb.append('\n'); j += 2 }
          case 114 => { sb.append('\r'); j += 2 }
          case 116 => { sb.append('\t'); j += 2 }

          case 34 => { sb.append('"'); j += 2 }
          case 47 => { sb.append('/'); j += 2 }
          case 92 => { sb.append('\\'); j += 2 }

          // if there's a problem then descape will explode
          case 117 => { sb.append(descape(at(j + 2, j + 6))); j += 6 }

          case c => die(j, s"invalid escape sequence (\\${c.toChar})")
        }
      } else if (c < 32) {
        die(j, s"control char ($c) in string")
      } else if (c < 128) {
        // 1-byte UTF-8 sequence
        sb.append(c.toChar)
        j += 1
      } else if ((c & 224) == 192) {
        // 2-byte UTF-8 sequence
        sb.extend(at(j, j + 2))
        j += 2
      } else if ((c & 240) == 224) {
        // 3-byte UTF-8 sequence
        sb.extend(at(j, j + 3))
        j += 3
      } else if ((c & 248) == 240) {
        // 4-byte UTF-8 sequence
        sb.extend(at(j, j + 4))
        j += 4
      } else {
        die(j, "invalid UTF-8 encoding")
      }
      c = byte(j) & 0xff
    }
    if (key) plate.nestMap(sb.makeString) else plate.str(sb.makeString)
    j + 1
  }
}
