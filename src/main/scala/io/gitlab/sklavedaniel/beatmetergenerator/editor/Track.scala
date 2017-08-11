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
import javax.swing.text.StyleConstants

import io.gitlab.sklavedaniel.beatmetergenerator.beatmeters.FlyingBeatmeter2
import io.gitlab.sklavedaniel.beatmetergenerator.beatmeters.WaveformBeatmeter2
import io.gitlab.sklavedaniel.beatmetergenerator.utils.ObservableIntervalMap
import io.gitlab.sklavedaniel.beatmetergenerator.utils.ObservableIntervalMap.Unscalable

import scala.collection.JavaConverters
import scala.util.{Success, Try}
import scalafx.beans.binding.{Bindings, ObjectBinding}
import scalafx.beans.property._
import scalafx.beans.value.ObservableValue
import scalafx.collections.ObservableBuffer
import scalafx.scene.paint.Color
import scalafx.scene.text.Font

final class Tracks(val undoManager: Option[UndoManager]) {
  val content = ObservableBuffer[Track]()
  UndoManager.register(undoManager, content)
  val audio = ObjectProperty[Option[(URI, Array[Short])]](None)
  UndoManager.register(undoManager, audio)

  val flying = BooleanProperty(true)
  UndoManager.register(undoManager, flying)
  val flyingBeatmeter = ObjectProperty(FlyingBeatmeter2.Conf(1280, 40, 25.0, 0.3, 0.4, Color.web("#2a98ff"), Color.Black,
    Color.web("#ff3e2f"), Color.Black, ("Courgette", 60, true, false), 5, Color.web("#2a98ff"), Color.Black, 1.0, AlignCenter, 0.5, None))
  UndoManager.register(undoManager, flyingBeatmeter)
  val waveformBeatmeter = ObjectProperty(WaveformBeatmeter2.Conf(1280, 40, 25.0, 0.3, 0.4, 1.0, 0.0, Color.web("#2a98ff"), Color.web("#ff3e2f"),
    Color.Transparent, Color.web("#070707BB"), Color.web("#ffe400"),
    ("Courgette", 60, true, false), 5, Color.web("#2a98ff"), Color.Black, 1.0, AlignCenter, 0.5,
     None))
  UndoManager.register(undoManager, waveformBeatmeter)

  def toImmutable(base: URI) = {
    ImmutableTracks(content.toList.map(_.toImmutable(base)), audio().map(u => base.relativize(u._1)), flying(), flyingBeatmeter(), waveformBeatmeter())
  }

}

sealed trait Align

case object AlignLeft extends Align

case object AlignRight extends Align

case object AlignCenter extends Align


case class ImmutableTracks(content: List[ImmutableTrack], audio: Option[URI], flying: Boolean, flyingBeatmeter: FlyingBeatmeter2.Conf,
  waveformBeatmeter: WaveformBeatmeter2.Conf) {
  def toMutable(base: URI, load: URI => Try[Array[Short]], undoManager: Option[UndoManager]): Try[Tracks] = {
    (audio match {
      case Some(uri) =>
        val auri = base.resolve(uri)
        load(uri).map(arr => Some((auri, arr)))
      case None =>
        Success(None)
    }).flatMap { aud =>
      val list = content.map(_.toMutable(base, load, undoManager)).foldLeft(Success(Nil): Try[List[Track]]) { (l, t) =>
        l.flatMap(l2 => t.map(t2 => t2 :: l2))
      }
      list.map { l =>
        val t = new Tracks(undoManager)
        undoManager.foreach(_.active = false)
        t.content ++= l.reverse
        t.audio() = aud
        t.flyingBeatmeter() = flyingBeatmeter
        t.waveformBeatmeter() = waveformBeatmeter
        t.flying() = flying
        undoManager.foreach(_.active = true)
        t
      }
    }
  }
}

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

case class ImmutableTrack(title: String, play: Boolean, record: Boolean, display: Boolean, snap: Boolean,
  content: List[(Double, Double, ImmutableTrackElement)], beat: Option[URI]
) {
  def toMutable(base: URI, load: URI => Try[Array[Short]], undoManager: Option[UndoManager]) = {
    (beat match {
      case Some(uri) =>
        val auri = base.resolve(uri)
        load(uri).map(arr => Some((auri, arr)))
      case None =>
        Success(None)
    }).map { aud =>
      val tmp = new Track(undoManager)
      val cntnt = content.map { elem =>
        (elem._1, elem._2, elem._3.toMutable(undoManager))
      }
      undoManager.foreach(_.active = false)
      tmp.title() = title
      tmp.play() = play
      tmp.record() = record
      tmp.display() = display
      tmp.snap() = snap
      tmp.content ++= cntnt
      tmp.beat() = aud
      undoManager.foreach(_.active = true)
      tmp
    }
  }
}

sealed trait TrackElement {
  def undoManager: Option[UndoManager]

  def toImmutable(): ImmutableTrackElement
}

sealed trait ImmutableTrackElement {
  def toMutable(undoManager: Option[UndoManager]): TrackElement
}


class Beat(override val undoManager: Option[UndoManager]) extends TrackElement with Unscalable[Double] {
  val highlight = BooleanProperty(false)
  UndoManager.register(undoManager, highlight)
  val duration = 0.05

  override def toImmutable() = ImmutableBeat(highlight())
}

case class ImmutableBeat(highlight: Boolean) extends ImmutableTrackElement {
  override def toMutable(undoManager: Option[UndoManager]) = {
    val b = new Beat(undoManager)
    undoManager.foreach(_.active = false)
    b.highlight() = highlight
    undoManager.foreach(_.active = true)
    b
  }
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

case class ImmutableBPMPattern(highlightFirst: Boolean, bpm: Double, beat: ImmutableBeat) extends ImmutableTrackElement {
  override def toMutable(undoManager: Option[UndoManager]) = {
    val b = new BPMPattern(beat.toMutable(undoManager), undoManager)
    undoManager.foreach(_.active = false)
    b.bpm() = bpm
    b.highlightFirst() = highlightFirst
    undoManager.foreach(_.active = true)
    b
  }
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

case class ImmutableBeatsPattern(highlightFirstPattern: Boolean, highlightFirst: Boolean, patternDuration: Double, pattern: List[(Double, Double, ImmutableBeat)]) extends ImmutableTrackElement {
  override def toMutable(undoManager: Option[UndoManager]) = {
    val b = new BeatsPattern(undoManager)
    val pttrn = pattern.map(x => (x._1, x._2, x._3.toMutable(undoManager)))
    undoManager.foreach(_.active = false)
    b.highlightFirstPattern() = highlightFirstPattern
    b.highlightFirst() = highlightFirst
    b.patternDuration() = patternDuration
    b.pattern ++= pttrn
    undoManager.foreach(_.active = true)
    b
  }
}

class Message(override val undoManager: Option[UndoManager]) extends TrackElement {
  val text = ObjectProperty("")
  UndoManager.register(undoManager, text)

  override def toImmutable = ImmutableMessage(text())
}

case class ImmutableMessage(text: String) extends ImmutableTrackElement {
  override def toMutable(undoManager: Option[UndoManager]) = {
    val b = new Message(undoManager)
    undoManager.foreach(_.active = false)
    b.text() = text
    undoManager.foreach(_.active = true)
    b
  }
}

object UndoManager {
  def register[A, B](undoManager: Option[UndoManager], property: Property[A, B]) = {
    undoManager.foreach(um => {
      property.onChange(um.propertyAction(property))
    })
  }

  def register[A](undoManager: Option[UndoManager], list: ObservableBuffer[A]) = {
    undoManager.foreach(um => {
      list.onChange(um.listAction(list))
    })
  }

  def register[A, B](undoManager: Option[UndoManager], map: ObservableIntervalMap[A, B]) = {
    undoManager.foreach(um => {
      map.addListener(um.intervalMapAction(map))
    })
  }
}

final class UndoManager {

  private var undoActions = List[(() => Unit, () => Unit)]()
  private var redoActions = List[(() => Unit, () => Unit)]()
  var active = true
  private var group: Option[List[(() => Unit, () => Unit)]] = None

  def clear(): Unit = {
    undoActions = Nil
    redoActions = Nil
    group = None
    undoable_() = false
    redoable_() = false
  }

  def startGroup(): Unit = {
    group = Some(Nil)
  }

  def endGroup(): Unit = {
    val list = group.get
    group = None
    if (list.nonEmpty) {
      doAction(() => {
        list.reverse.foreach(_._1())
      }, () => {
        list.foreach(_._2())
      })
    }
  }

  def propertyAction[A, B](property: Property[A, B]) = (_: ObservableValue[A, B], old: B, current: B) => {
    if (old != current) {
      doAction(() => {
        property.setValue(current)
      }, () => {
        property.setValue(old)
      })
    }
  }

  def listAction[A](list: ObservableBuffer[A]) = (_: ObservableBuffer[A], cs: Seq[ObservableBuffer.Change[A]]) => {
    if (cs.nonEmpty) {
      val changes = for (c <- cs) yield
        c match {
          case ObservableBuffer.Add(pos, elems) =>
            Left((pos, JavaConverters.asJavaCollection(elems.toList)))
          case ObservableBuffer.Remove(pos, elems) =>
            Right((pos, JavaConverters.asJavaCollection(elems.toList)))
          case _ => assert(false); ???
        }
      doAction(() => {
        for (c <- changes) {
          c match {
            case Left((pos, elems)) =>
              list.addAll(pos, elems)
            case Right((pos, elems)) =>
              list.remove(pos, elems.size)
          }
        }
      }, () => {
        for (c <- changes.reverse) {
          c match {
            case Left((pos, elems)) =>
              list.remove(pos, elems.size)
            case Right((pos, elems)) =>
              list.addAll(pos, elems)
            case _ => assert(false)
          }
        }
      })
    }
  }

  def intervalMapAction[A, B](map: ObservableIntervalMap[A, B]): ObservableIntervalMap.ChangeListener[A, B] = (_: ObservableIntervalMap[A, B], change: ObservableIntervalMap.Change[A, B]) => {
    change match {
      case ObservableIntervalMap.AddChange(cs) =>
        if (cs.nonEmpty) {
          doAction(() => {
            map ++= cs
          }, () => {
            map --= cs.map(c => (c._1, c._2))
          })
        }
      case ObservableIntervalMap.RemoveChange(cs) => {
        if (cs.nonEmpty) {
          doAction(() => {
            map --= cs.map(c => (c._1, c._2))
          }, () => {
            map ++= cs
          })
        }
      }
      case ObservableIntervalMap.MoveChange(from, to, cs) =>
        if (cs.nonEmpty) {
          doAction(() => {
            map --= cs.map(c => c._1)
            map ++= cs.map(c => (c._2._1, c._2._2, c._3))
          }, () => {
            map --= cs.map(c => c._2)
            map ++= cs.map(c => (c._1._1, c._1._2, c._3))
          })
        }
    }
  }

  def doAction(undo: () => Unit, redo: () => Unit): Unit = {
    if (active) {
      if (group.isDefined) {
        group = Some((undo, redo) :: group.get)
      } else {
        undoActions = (undo, redo) :: undoActions
        redoActions = Nil
        redoable_() = false
        undoable_() = true
      }
    }
  }

  def undoAction(): Unit = {
    assert(group.isEmpty)
    if (undoActions.nonEmpty) {
      active = false
      val action = undoActions.head
      undoActions = undoActions.tail
      redoActions = action :: redoActions
      action._2()
      redoable_() = true
      undoable_() = undoActions.nonEmpty
      active = true
    }
  }

  def redoAction(): Unit = {
    assert(group.isEmpty)
    if (redoActions.nonEmpty) {
      active = false
      val action = redoActions.head
      redoActions = redoActions.tail
      undoActions = action :: undoActions
      action._1()
      redoable_() = redoActions.nonEmpty
      undoable_() = true
      active = true
    }
  }

  private val undoable_ = ReadOnlyBooleanWrapper(false)
  val undoable = undoable_.readOnlyProperty
  private val redoable_ = ReadOnlyBooleanWrapper(false)
  val redoable = redoable_.readOnlyProperty

}
