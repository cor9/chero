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

package io.gitlab.sklavedaniel.beatmetergenerator.editor

import io.gitlab.sklavedaniel.beatmetergenerator.utils.ObservableIntervalMap

import scalafx.beans.binding.{Bindings, ObjectBinding}
import scalafx.beans.property.{BooleanProperty, DoubleProperty, ObjectProperty}

class Track {
  val title = ObjectProperty("")
  val content = ObservableIntervalMap[Double, TrackElement]
}

sealed trait TrackElement

class Beat() extends TrackElement {
  val name = ObjectProperty("")
  val highlight = BooleanProperty(false)
}

sealed trait BeatsGenerator extends TrackElement {
  def beats: ObjectBinding[Stream[(Double, Double, Beat)]]
}

class BPMPattern() extends BeatsGenerator {
  val bpm = DoubleProperty(0.0)
  val beat = new Beat()
  val beats = Bindings.createObjectBinding(() => if (bpm() == 0) {
    Stream.empty
  } else {
    Stream.from(0).map { i =>
      (i * 60 / bpm(), i * 60 / bpm() + 0.05, beat)
    }
  }, bpm)
}

class BeatsPattern() extends BeatsGenerator {
  val patternDuration = DoubleProperty(0.0)
  val pattern = ObservableIntervalMap[Double, Beat]
  val beats = Bindings.createObjectBinding(() => {
    Stream.from(0).flatMap { i =>
      pattern.map { b =>
        (i * patternDuration() + b._1, i * patternDuration() + b._2, b._3)
      }
    }
  }, pattern, patternDuration)
}

class Message() extends TrackElement {
  val text = ObjectProperty("")
}
