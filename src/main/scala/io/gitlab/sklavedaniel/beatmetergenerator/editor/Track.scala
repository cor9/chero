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

sealed trait TrackElement {
  def toImmutable(): ImmutableTrackElement
}

sealed trait ImmutableTrackElement {
  def toMutable(): TrackElement
}


class Beat() extends TrackElement {
  val name = ObjectProperty("")
  val highlight = BooleanProperty(false)

  override def toImmutable() = ImmutableBeat(name(), highlight())
}

case class ImmutableBeat(name: String, highlight: Boolean) extends ImmutableTrackElement {
  override def toMutable() = {
    val b = new Beat()
    b.name() = name
    b.highlight() = highlight
    b
  }
}

sealed trait BeatsGenerator extends TrackElement {
  def beats: ObjectBinding[Stream[(Double, Double, Beat)]]
}

class BPMPattern(val beat: Beat) extends BeatsGenerator {
  def this() {
    this(new Beat())
  }

  val bpm = DoubleProperty(0.0)
  val beats = Bindings.createObjectBinding(() => if (bpm() == 0) {
    Stream.empty
  } else {
    Stream.from(0).map { i =>
      (i * 60 / bpm(), i * 60 / bpm() + 0.05, beat)
    }
  }, bpm)

  override def toImmutable() = ImmutableBPMPattern(bpm(), beat.toImmutable())
}

case class ImmutableBPMPattern(bpm: Double, beat: ImmutableBeat) extends ImmutableTrackElement {
  override def toMutable() = {
    val b = new BPMPattern(beat.toMutable())
    b.bpm() = bpm
    b
  }
}

class BeatsPattern() extends BeatsGenerator {
  val patternDuration = DoubleProperty(0.0)
  val pattern = ObservableIntervalMap[Double, Beat]
  val beats = Bindings.createObjectBinding(() => {
    if (pattern.isEmpty) {
      Stream.empty
    } else {
      Stream.from(0).flatMap { i =>
        pattern.map { b =>
          (i * patternDuration() + b._1, i * patternDuration() + b._2, b._3)
        }
      }
    }
  }, pattern, patternDuration)

  override def toImmutable = ImmutableBeatsPattern(patternDuration(), pattern.toList.map(x => (x._1, x._2, x._3.toImmutable())))
}

case class ImmutableBeatsPattern(patternDuration: Double, pattern: List[(Double, Double, ImmutableBeat)]) extends ImmutableTrackElement {
  override def toMutable = {
    val b = new BeatsPattern()
    b.patternDuration() = patternDuration
    b.pattern ++= pattern.map(x => (x._1, x._2, x._3.toMutable()))
    b
  }
}

class Message() extends TrackElement {
  val text = ObjectProperty("")

  def copy(): Message = {
    val b = new Message()
    b.text() = text()
    b
  }

  override def toImmutable = ImmutableMessage(text())
}

case class ImmutableMessage(text: String) extends ImmutableTrackElement {
  override def toMutable = {
    val b = new Message()
    b.text() = text
    b
  }
}
