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

package io.gitlab.sklavedaniel.beatmetergenerator.utils

import java.io.File
import java.net.URI

import io.gitlab.sklavedaniel.beatmetergenerator.beatmeters.{FlyingBeatmeter, WaveformBeatmeter}
import io.gitlab.sklavedaniel.beatmetergenerator.utils.ImmutableTracks_V0_2_0._
import io.gitlab.sklavedaniel.beatmetergenerator.utils.ImmutableTracks_V0_2_3.ImmutableTracks
import io.gitlab.sklavedaniel.beatmetergenerator.utils.ObservableIntervalMap.Unscalable

import scala.util.{Failure, Success, Try}
import scalafx.beans.binding.{Bindings, ObjectBinding}
import scalafx.beans.property._
import scalafx.collections.ObservableBuffer
import scalafx.scene.paint.Color

final class Tracks(val undoManager: Option[UndoManager]) {
  val content = ObservableBuffer[Track]()
  UndoManager.register(undoManager, content)
  val audio = ObjectProperty[Option[(URI, Array[Short])]](None)
  UndoManager.register(undoManager, audio)

  val flying = BooleanProperty(true)
  UndoManager.register(undoManager, flying)
  val flyingBeatmeter = ObjectProperty(FlyingBeatmeter.Conf_V0_2_3(1280, 40, 25.0, 0.3, 0.4, Color.web("#2a98ff"), Color.Black,
    Color.web("#ff3e2f"), Color.Black, 1.5, 0.2, Color.Black, Color.web("#00000088"), ("Courgette", 60, true, false), 5, Color.web("#2a98ff"), Color.Black, 1.0, AlignCenter, 0.5, None))
  UndoManager.register(undoManager, flyingBeatmeter)
  val waveformBeatmeter = ObjectProperty(WaveformBeatmeter.Conf_V0_2_0(1280, 40, 25.0, 0.3, 0.4, 1.0, 0.0, Color.web("#2a98ff"), Color.web("#ff3e2f"),
    Color.Transparent, Color.web("#070707BB"), Color.web("#ffe400"),
    ("Courgette", 60, true, false), 5, Color.web("#2a98ff"), Color.Black, 1.0, AlignCenter, 0.5,
    None))
  UndoManager.register(undoManager, waveformBeatmeter)

  val file = ObjectProperty[Option[File]](None)

  def toImmutable(base: URI) = {
    ImmutableTracks(content.toList.map(_.toImmutable(base)), audio().map(u => base.relativize(u._1)), flying(), flyingBeatmeter(), waveformBeatmeter())
  }

}

sealed trait AlignH

case object AlignLeft extends AlignH

case object AlignRight extends AlignH

case object AlignCenter extends AlignH

sealed trait AlignV

case object AlignTop extends AlignV

case object AlignBottom extends AlignV

case object AlignMiddle extends AlignV


class Track(val undoManager: Option[UndoManager]) {
  val title = ObjectProperty("")
  UndoManager.register(undoManager, title)
  val play = BooleanProperty(false)
  UndoManager.register(undoManager, play)
  val record = BooleanProperty(false)
  UndoManager.register(undoManager, record)
  val display = BooleanProperty(false)
  UndoManager.register(undoManager, display)
  val snap = BooleanProperty(false)
  UndoManager.register(undoManager, snap)
  val content = ObservableIntervalMap[Double, TrackElement]
  UndoManager.register(undoManager, content)
  val beat = ObjectProperty[Option[(URI, Array[Short])]](None)
  UndoManager.register(undoManager, beat)

  def toImmutable(base: URI) = {
    new ImmutableTrack(title(), play(), record(), display(), snap(), content.toList.map { elem =>
      (elem._1, elem._2, elem._3.toImmutable())
    }, beat().map(u => base.relativize(u._1)))
  }
}


sealed trait TrackElement {
  def undoManager: Option[UndoManager]

  def toImmutable(): ImmutableTrackElement
}


class Beat(override val undoManager: Option[UndoManager]) extends TrackElement with Unscalable[Double] {
  val highlight = BooleanProperty(false)
  UndoManager.register(undoManager, highlight)
  val duration = 0.05

  override def toImmutable() = ImmutableBeat(highlight())
}


sealed trait BeatsGenerator extends TrackElement {
  def beats: ObjectBinding[Stream[(Double, Double, Beat)]]
}

class BPMPattern(val beat: Beat, override val undoManager: Option[UndoManager]) extends BeatsGenerator {
  def this(undoManager: Option[UndoManager]) {
    this(new Beat(undoManager), undoManager)
  }

  val highlightFirst = BooleanProperty(false)
  UndoManager.register(undoManager, highlightFirst)
  val bpm = DoubleProperty(0.0)
  UndoManager.register(undoManager, bpm)
  private val firstBeat = new Beat(None)
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


class BeatsPattern(override val undoManager: Option[UndoManager]) extends BeatsGenerator {
  val highlightFirstPattern = BooleanProperty(false)
  UndoManager.register(undoManager, highlightFirstPattern)
  val highlightFirst = BooleanProperty(false)
  UndoManager.register(undoManager, highlightFirst)
  val patternDuration = DoubleProperty(0.0)
  UndoManager.register(undoManager, patternDuration)
  val pattern = ObservableIntervalMap[Double, Beat]
  UndoManager.register(undoManager, pattern)
  val beats = Bindings.createObjectBinding(() => {
    if (pattern.isEmpty) {
      Stream.empty
    } else {
      val tmp = Stream.from(0).flatMap { i =>
        pattern.map { b =>
          (i * patternDuration() + b._1, i * patternDuration() + b._2, b._3)
        }
      }
      if (highlightFirstPattern()) {
        val bs = pattern.map { b =>
          val b3 = b._3.toImmutable().toMutable(None)
          b3.highlight() = true
          (b._1, b._2, b3)
        }
        bs.toStream ++ tmp.drop(pattern.size)
      } else if (highlightFirst()) {
        val b = pattern.head._3.toImmutable().toMutable(None)
        b.highlight() = true
        Stream((pattern.head._1, pattern.head._2, b)) ++ tmp.tail
      } else {
        tmp
      }
    }
  }, pattern, patternDuration, highlightFirst, highlightFirstPattern)

  override def toImmutable = ImmutableBeatsPattern(highlightFirstPattern(), highlightFirst(), patternDuration(), pattern.toList.map(x => (x._1, x._2, x._3.toImmutable())))
}

class Message(override val undoManager: Option[UndoManager]) extends TrackElement {
  val text = ObjectProperty("")
  UndoManager.register(undoManager, text)

  override def toImmutable = ImmutableMessage(text())
}

case class WithFailures[+A, +B](value: Option[A], failures: List[B]) {
  def flatMap[A2, B2 >: B](f: A => WithFailures[A2, B2]): WithFailures[A2, B2] = {
    val tmp = value match {
      case Some(v) => f(v)
      case None => WithFailures(None, Nil)
    }
    WithFailures(tmp.value, failures ++ tmp.failures)
  }
  def withDefault[A2 >: A](a: => A2): WithFailures[A2, B] = value match {
    case Some(_) => this
    case None => WithFailures(Some(a), failures)
  }
  def map[A2](f: A => A2): WithFailures[A2, B] = flatMap(x => WithFailures(Some(f(x)), Nil))
  def get = value.get
}

object WithFailures {

  def success[A](value: A) = WithFailures(Some(value), Nil)

  def failure[B](failure: B) = WithFailures(None, List(failure))

  def withFailures[A](value: => A) = try {
    WithFailures(Some(value), Nil)
  } catch {
    case e: Throwable =>
      WithFailures(None, List(e))
  }

  def fromTry[A](value: Try[A]): WithFailures[A, Throwable] = value match {
    case Success(v) => success(v)
    case Failure(e) => failure(e)
  }
}



