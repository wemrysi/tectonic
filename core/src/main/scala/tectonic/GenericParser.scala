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

import scala.{Array, Byte}
import scala.util.Either

import java.lang.{String, SuppressWarnings}
import java.nio.ByteBuffer
import java.nio.charset.Charset

trait GenericParser[A] {

  def absorb(buf: ByteBuffer): Either[ParseException, A]
  def finish(): Either[ParseException, A]

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  final def absorb(bytes: Array[Byte]): Either[ParseException, A] =
    absorb(ByteBuffer.wrap(bytes))

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  final def absorb(s: String): Either[ParseException, A] =
    absorb(ByteBuffer.wrap(s.getBytes(GenericParser.Utf8)))
}

private[tectonic] object GenericParser {
  val Utf8 = Charset.forName("UTF-8")
}
