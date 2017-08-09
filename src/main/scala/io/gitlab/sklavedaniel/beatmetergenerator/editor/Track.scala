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

import java.net.URI

import io.gitlab.sklavedaniel.beatmetergenerator.beatmeters.FlyingBeatmeter2
import io.gitlab.sklavedaniel.beatmetergenerator.utils.ObservableIntervalMap

import scala.util.{Success, Try}
import scalafx.beans.binding.{Bindings, ObjectBinding}
import scalafx.beans.property.{BooleanProperty, DoubleProperty, ObjectProperty}
import scalafx.collections.ObservableBuffer
import scalafx.scene.paint.Color

final class Tracks {
  val content = ObservableBuffer[Track]()
  val audio = ObjectProperty[Option[(URI, Array[Short])]](None)
  val beatmeterSettings = ObjectProperty(FlyingBeatmeter2.Conf(1280, 40, 25.0, 0.2, 0.4, Color.DarkRed, Color.DarkGray,
    Color.Yellow, Color.DarkGray, None, None, 50, 5, Color.Red))

  def toImmutable(base: URI) = {
    ImmutableTracks(content.toList.map(_.toImmutable(base)), audio().map(u => base.relativize(u._1)), beatmeterSettings())
  }
}

case class ImmutableTracks(content: List[ImmutableTrack], audio: Option[URI], beatmeterSettings: FlyingBeatmeter2.Conf) {
  def toMutable(base: URI, load: URI => Try[Array[Short]]): Try[Tracks] = {
    (audio match {
      case Some(uri) =>
        val auri = base.resolve(uri)
        load(uri).map(arr => Some((auri, arr)))
      case None =>
        Success(None)
    }).flatMap { aud =>
      val list = content.map(_.toMutable(base, load)).foldLeft(Success(Nil): Try[List[Track]]) { (l, t) =>
        l.flatMap(l2 => t.map(t2 => t2 :: l2))
      }
      list.map { l =>
        val t = new Tracks()
        t.content ++= l.reverse
        t.audio() = aud
        t.beatmeterSettings() = beatmeterSettings
        t
      }
    }
  }
}

class Track {
  val title = ObjectProperty("")
  val play = BooleanProperty(false)
  val record = BooleanProperty(false)
  val display = BooleanProperty(false)
  val snap = BooleanProperty(false)
  val content = ObservableIntervalMap[Double, TrackElement]
  val beat = ObjectProperty[Option[(URI, Array[Short])]](None)

  def toImmutable(base: URI) = {
    new ImmutableTrack(title(), play(), record(), display(), snap(), content.toList.map { elem =>
      (elem._1, elem._2, elem._3.toImmutable())
    }, beat().map(u => base.relativize(u._1)))
  }
}

case class ImmutableTrack(title: String, play: Boolean, record: Boolean, display: Boolean, snap: Boolean,
  content: List[(Double, Double, ImmutableTrackElement)], beat: Option[URI]
) {
  def toMutable(base: URI, load: URI => Try[Array[Short]]) = {
    (beat match {
      case Some(uri) =>
        val auri = base.resolve(uri)
        load(uri).map(arr => Some((auri, arr)))
      case None =>
        Success(None)
    }).map { aud =>
      val tmp = new Track()
      tmp.title() = title
      tmp.play() = play
      tmp.record() = record
      tmp.display() = display
      tmp.snap() = snap
      tmp.content ++= content.map { elem =>
        (elem._1, elem._2, elem._3.toMutable())
      }
      tmp.beat() = aud
      tmp
    }
  }
}

sealed trait TrackElement {
  def toImmutable(): ImmutableTrackElement
}

trait Unscalable[A] {
  def duration: A
}

sealed trait ImmutableTrackElement {
  def toMutable(): TrackElement
}


class Beat() extends TrackElement with Unscalable[Double] {
  val highlight = BooleanProperty(false)
  val duration = 0.05

  override def toImmutable() = ImmutableBeat(highlight())
}

case class ImmutableBeat(highlight: Boolean) extends ImmutableTrackElement {
  override def toMutable() = {
    val b = new Beat()
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

  val highlightFirst = BooleanProperty(false)
  val bpm = DoubleProperty(0.0)
  private val firstBeat = new Beat()
  firstBeat.highlight <== highlightFirst
  val beats = Bindings.createObjectBinding(() => if (bpm() == 0) {
    Stream.empty
  } else {
    Stream((0.0, 0.05, firstBeat)) ++ Stream.from(1).map { i =>
      (i * 60 / bpm(), i * 60 / bpm() + 0.05, beat)
    }
  }, bpm)

  override def toImmutable() = ImmutableBPMPattern(highlightFirst(), bpm(), beat.toImmutable())
}

case class ImmutableBPMPattern(highlightFirst: Boolean, bpm: Double, beat: ImmutableBeat) extends ImmutableTrackElement {
  override def toMutable() = {
    val b = new BPMPattern(beat.toMutable())
    b.bpm() = bpm
    b.highlightFirst() = highlightFirst
    b
  }
}

class BeatsPattern() extends BeatsGenerator {
  val highlightFirst = BooleanProperty(false)
  val patternDuration = DoubleProperty(0.0)
  val pattern = ObservableIntervalMap[Double, Beat]
  val beats = Bindings.createObjectBinding(() => {
    if (pattern.isEmpty) {
      Stream.empty
    } else {
      val tmp = Stream.from(0).flatMap { i =>
        pattern.map { b =>
          (i * patternDuration() + b._1, i * patternDuration() + b._2, b._3)
        }
      }
      if (highlightFirst()) {
        val b = pattern.head._3.toImmutable().toMutable()
        b.highlight() = true
        Stream((pattern.head._1, pattern.head._2, b)) ++ tmp.tail
      } else {
        tmp
      }
    }
  }, pattern, patternDuration, highlightFirst)

  override def toImmutable = ImmutableBeatsPattern(highlightFirst(), patternDuration(), pattern.toList.map(x => (x._1, x._2, x._3.toImmutable())))
}

case class ImmutableBeatsPattern(highlightFirst: Boolean, patternDuration: Double, pattern: List[(Double, Double, ImmutableBeat)]) extends ImmutableTrackElement {
  override def toMutable = {
    val b = new BeatsPattern()
    b.highlightFirst() = highlightFirst
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


