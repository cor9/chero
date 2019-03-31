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

import io.gitlab.sklavedaniel.beatmetergenerator.editor.BeatEditor._
import io.gitlab.sklavedaniel.beatmetergenerator.utils.ImmutableTracks_V0_2_0.ImmutableTrackElement
import io.gitlab.sklavedaniel.beatmetergenerator.utils._
import scalafx.Includes._
import scalafx.beans.binding.{Bindings, ObjectBinding}
import scalafx.beans.property.{DoubleProperty, ObjectProperty}
import scalafx.geometry.Side
import scalafx.scene.Group
import scalafx.scene.control._
import scalafx.scene.input.{MouseButton, MouseEvent}
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.scene.transform.Scale

import scala.collection.mutable
import Utils._

class TrackView(initPxPerSec: Double, val track: Track, snaps: ObjectProperty[Option[ObservableIntervalMap[Double, Beat]]],
  override val undoManager: UndoManager, audio: ObjectProperty[Option[Array[Short]]],
  digitDown: ObjectBinding[Option[Int]], override val selectionContainer: ObjectProperty[Option[SelectionContainer[_]]]
) extends Group with SelectionContainer[TrackElement] {
  self =>
  val content = track.content
  val clazz = classOf[ImmutableTrackElement]
  val duration = DoubleProperty(0.0)
  override val pxPerSec = DoubleProperty(initPxPerSec)
  val beats = ObservableIntervalMap[Double, Beat]

  override val scaled = {
    val s = new Scale(1.0, 1.0, 0.0, 0.0)
    transforms = Seq(s)
    s.x
  }
  val labelX = DoubleProperty(0.0)

  val elementGroup = new Group {
    layoutY = 25
    layoutX = 0
  }
  val element2view = mutable.Map[(Double, TrackElement), TrackElementView[TrackElement, TrackElement]]()

  override def getView(key: (Double, TrackElement)): TrackElementView[TrackElement, TrackElement] = element2view(key)

  override def startPosition = 0.0

  def snap(pos: Double, offset: Double, atStart: Boolean) = (digitDown() match {
    case None => Some(0)
    case Some(0) => None
    case s => s
  }).flatMap {
    extraSnaps =>
      snaps().flatMap {
        snaps =>
          val (start, end) = if (atStart) {
            (snaps.starting((pos + offset - 10).max(0.0), pos + offset).lastOption.map(_._1 - offset),
              snaps.starting(pos + offset, pos + offset + 10).headOption.map(_._1 - offset))
          } else {
            (snaps.ending((pos + offset - 10).max(0.0), pos + offset).lastOption.map(_._2 - offset),
              snaps.ending(pos + offset, pos + offset + 10).headOption.map(_._2 - offset))
          }
          val points = (start match {
            case Some(s) => end match {
              case Some(e) =>
                Iterator(s) ++ Iterator.range(1, extraSnaps + 1).map {
                  i => s + i * (e - s) / (extraSnaps + 1)
                } ++ Iterator(e)
              case None => Iterator(s)
            }
            case None => end.toIterator
          }).filter(x => (x - pos).abs <= 0.1)
          if (points.isEmpty) {
            None
          } else {
            Some(points.minBy(x => (x - pos).abs))
          }
      }
  }.getOrElse(pos)

  private def addGenerator(gen: BeatsGenerator, tmp: TrackElementView[TrackElement, TrackElement]): Unit = {
    beats ++= gen.beats().map(e => (e._1 + tmp.position()._1, e._2 + tmp.position()._1, e._3)).takeWhile(_._2 <= tmp.position()._2)
    gen.beats.onInvalidate {
      beats --= beats.intersecting(tmp.position()._1, tmp.position()._2).map(e => (e._1, e._2))
      beats ++= gen.beats().map(e => (e._1 + tmp.position()._1, e._2 + tmp.position()._1, e._3)).takeWhile(_._2 <= tmp.position()._2)
    }
    tmp.position.onChange { (_, old, pos) =>
      beats --= beats.intersecting(old._1, old._2).map(e => (e._1, e._2))
      beats ++= gen.beats().map(e => (e._1 + tmp.position()._1, e._2 + tmp.position()._1, e._3)).takeWhile(_._2 <= tmp.position()._2)
    }
  }

  private def addElement(elem: (Double, Double, TrackElement)): Unit = {
    val v = elem._3 match {
      case beat: Beat =>
        beats ++= Seq((elem._1, elem._2, beat))
        new BeatView[TrackElement](beat, true, self)
      case beatsGroup: BeatsPattern =>
        val tmp = new BeatsPatternView[TrackElement](beatsGroup, self)
        addGenerator(beatsGroup, tmp)
        tmp
      case bpmGroup: BPMPattern =>
        val tmp = new BPMPatternView(bpmGroup, self, audio)
        addGenerator(bpmGroup, tmp)
        tmp
      case message: Message =>
        new MessageView[TrackElement](message, self)
    }
    v.pxPerSec <== pxPerSec
    v.position = (elem._1, elem._2)
    v.position.onChange { (_, old, pos) =>
      if (old != pos && !track.content.contains(pos._1, pos._2)) track.content.move(old, pos)
    }
    v.layoutX <== Bindings.createDoubleBinding(() => pxPerSec() * v.position()._1, pxPerSec, v.position)
    element2view((elem._1, elem._3)) = v
    elementGroup.children.add(v)
  }

  private val listener = ObservableIntervalMap.WeakChangeListner[Double, TrackElement] {
    case (_, ObservableIntervalMap.AddChange(l: List[(Double, Double, TrackElement)])) =>
      for (elem <- l) {
        addElement(elem)
      }
    case (_, ObservableIntervalMap.MoveChange(from, to, l)) =>
      if (from != to) {
        for ((f, t, b) <- l) {
          val v = element2view.remove((f._1, b)).get
          element2view += (t._1, b) -> v
          v.position = t
        }
      }
    case (_, ObservableIntervalMap.RemoveChange(l: List[(Double, Double, TrackElement)])) =>
      for (elem <- l) {
        element2view.remove((elem._1, elem._3)).foreach { v =>
          elementGroup.children.remove(v)
          selectedElements.remove(v)
          beats --= beats.intersecting(elem._1, elem._2).map(e => (e._1, e._2))
        }
      }
  }
  track.content.addListener(listener)
  for (elem <- track.content) {
    addElement(elem)
  }

  var contextMenuX = 0.0

  def newResizableElement(elem: TrackElement): Unit = newResizableElement(contextMenuX, elem)
  def newResizableElement(pos: Double, elem: TrackElement): Unit = {
    val duration = content.starting(pos, pos + 5).headOption.map(x => 0.5.max(x._1 - pos - 0.5)).getOrElse(5.0)
    if (content.intersecting(pos, pos + duration).isEmpty) {
      content ++= Iterator((pos, pos + duration, elem))
    }
  }

  def newBeatPattern(duration: Double, beats: Seq[Double]): Unit = newBeatPattern(contextMenuX, duration, beats)

  def newBeatPattern(x: Double, duration: Double, beats: Seq[Double]): Unit = {
    val b = new BeatsPattern(Some(undoManager))
    val tmp = beats.map(d => (d, d + 0.05, new Beat(Some(undoManager))))
    undoManager.active = false
    b.pattern ++= tmp
    b.patternDuration() = duration
    undoManager.active = true
    newResizableElement(x, b)
  }

  val contextMenu: ContextMenu = new ContextMenu(
    new MenuItem("New Beat") {
      onAction = handle {
        val b = new Beat(Some(undoManager))
        if (content.intersecting(contextMenuX, contextMenuX + 0.05).isEmpty) {
          content ++= Iterator((contextMenuX, contextMenuX + 0.05, b))
        }
      }
    },
    new MenuItem("New BPM Pattern") {
      onAction = handle {
        val b = new BPMPattern(Some(undoManager))
        undoManager.active = false
        b.bpm() = 60
        undoManager.active = true
        newResizableElement(b)
      }
    },
    new MenuItem("New Beat Pattern") {
      onAction = handle {
        newBeatPattern(1.0, Seq(0.0))
      }
    },
    new MenuItem("New Message") {
      onAction = handle {
        val b = new Message(Some(undoManager))
        undoManager.active = false
        b.text() = "Hello!"
        undoManager.active = true
        newResizableElement(b)
      }
    },
    new MenuItem("Insert") {
      onAction = handle {
        insert(contextMenuX)
      }
    },
    new Menu("Common Patterns") {
      items = Seq(
        new MenuItem("123-") {
          onAction = handle {
            newBeatPattern(1.0, Seq(0.0, 0.25, 0.5))
          }
        },
        new MenuItem("123-4-5-") {
          onAction = handle {
            newBeatPattern(1.0, Seq(0.0, 0.125, 0.25, 0.5, 0.75))
          }
        },
        new MenuItem("12345-") {
          onAction = handle {
            newBeatPattern(1.5, Seq(0.0, 0.25, 0.5, 0.75, 1.0))
          }
        },
        new MenuItem("1234567-") {
          onAction = handle {
            newBeatPattern(2.0, Seq(0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5))
          }
        },
        new MenuItem("12------") {
          onAction = handle {
            newBeatPattern(2.0, Seq(0.0, 0.25))
          }
        },
        new MenuItem("123-4-5-6-7-8-9-") {
          onAction = handle {
            newBeatPattern(2.0, Seq(0.0, 0.125, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75))
          }
        },
        new MenuItem("12345-6-7-8-9-10-") {
          onAction = handle {
            newBeatPattern(2.0, Seq(0.0, 0.125, 0.25, 0.375, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75))
          }
        }
      )
    }
  )

  override def mouseClicked(e: MouseEvent) = {
    if (!e.isConsumed && MouseButton.Secondary.equals(e.getButton)) {
      contextMenuX = e.getX / pxPerSec()
      contextMenu.show(self, Side.Bottom, e.getX * scaled(), e.getY - layoutBounds().getHeight)
    } else {
      contextMenu.hide()
    }
    e.consume()
    true
  }

  children = Seq(
    new Rectangle {
      x = 0.0
      y = 0.0
      height = 50.0
      width <== duration * pxPerSec
      fill = Color.Transparent
    },
    new Line {
      startY = 25
      startX = 0
      endY = 25
      endX <== duration * pxPerSec
    },
    elementGroup
  )

  override def propagateDragOver(): Unit = {}
}