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

import java.lang.CharSequence

import scala.{Boolean, Int, Unit}

abstract class Plate[A] {
  def nul(): Signal
  def fls(): Signal
  def tru(): Signal
  def map(): Signal
  def arr(): Signal
  def num(s: CharSequence, decIdx: Int, expIdx: Int): Signal
  def str(s: CharSequence): Signal

  def nestMap(pathComponent: CharSequence): Signal
  def nestArr(): Signal
  def nestMeta(pathComponent: CharSequence): Signal

  def unnest(): Signal

  def finishRow(): Unit
  def finishBatch(terminal: Boolean): A
}
