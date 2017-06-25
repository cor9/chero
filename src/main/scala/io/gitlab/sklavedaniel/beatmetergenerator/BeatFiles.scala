/*
 *  This file is part of Beatmeter Generator.
 *
 *  Beatmeter Generator is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  Beatmeter Generator is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Beatmeter Generator.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package io.gitlab.sklavedaniel.beatmetergenerator

import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

object BeatFiles {
  def store(file: File, beats: Traversable[Double]): Unit = {
    val output = new BufferedWriter(new FileWriter(file))
    try {
      for (b <- beats) {
        output.write(s"$b $b beat\n")
      }
    } finally
      output.close()
  }

  def load(file: File): Seq[Double] = {
    val result = Source.fromFile(file).getLines().flatMap { l =>
      val parts = "\\s+".r.split(l)
      val time = parts(0).replace(",", ".").toDouble
      if (parts(2) == "beat") {
        Some(time)
      } else {
        None
      }
    }.toList
    assert(result == result.sorted, "Input file must be sorted.")
    result
  }
}

