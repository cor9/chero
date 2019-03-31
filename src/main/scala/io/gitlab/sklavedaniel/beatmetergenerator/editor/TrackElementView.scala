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

import io.gitlab.sklavedaniel.beatmetergenerator.bpmdetection.WaveletBPMDetection
import io.gitlab.sklavedaniel.beatmetergenerator.editor.BeatEditor._
import io.gitlab.sklavedaniel.beatmetergenerator.utils.ImmutableTracks_V0_2_0.ImmutableBeat
import io.gitlab.sklavedaniel.beatmetergenerator.utils.WithFailures.success
import io.gitlab.sklavedaniel.beatmetergenerator.utils._
import javafx.beans.InvalidationListener
import javafx.geometry.VPos
import javafx.scene.Cursor
import scalafx.Includes._
import scalafx.beans.binding.Bindings
import scalafx.beans.property.{BooleanProperty, DoubleProperty, ObjectProperty, ReadOnlyObjectWrapper}
import scalafx.geometry.Side
import scalafx.scene.control._
import scalafx.scene.input.{MouseButton, MouseEvent}
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.scene.text._
import scalafx.scene.{Group, Node}

import scala.collection.mutable
import Utils._

sealed class TrackElementView[+A <: TrackElement, B <: TrackElement](val element: A, val context: SelectionContainer[B]) extends Group {
  val pxPerSec = DoubleProperty(0.0)
  private val position_ = ReadOnlyObjectWrapper((0.0, 0.0))

  def position = position_.readOnlyProperty

  def position_=(value: (Double, Double)): Unit = {
    position_() = value
  }

  val duration = Bindings.createDoubleBinding(() => position()._2 - position()._1, position)
  val width = pxPerSec * duration
  val selected = BooleanProperty(false)
}

sealed abstract class ResizableElementView[A <: TrackElement, B <: TrackElement](element: A, context: SelectionContainer[B]) extends TrackElementView[A, B](element, context) {
  self =>
  def target: Node

  target.onMouseMoved = e => {
    if (e.getX > width.doubleValue() - 5 / context.scaled.doubleValue()) {
      self.setCursor(Cursor.H_RESIZE)
    } else {
      self.setCursor(Cursor.DEFAULT)
    }
  }

  target.onMouseDragged = e => {
    if (resizeOffset.isDefined) {
      val tmp = position()._1
      val newDuration = minDuration.max((e.getX + resizeOffset.get).max(0.05) / pxPerSec())

      val snapped = context.snap(tmp + newDuration, 0.0, false)

      resizeBox.foreach { r =>
        r.width = (snapped - tmp) * pxPerSec()
        if (context.content.intersecting(tmp, snapped).forall(p => tmp <= p._1 && p._2 <= position()._2)) {
          r.fill = Color.DarkBlue.opacity(0.5)
        } else {
          r.fill = Color.DarkRed.opacity(0.5)
        }
      }
      e.consume()
    }
  }

  def minDuration = 0.0

  private var resizeOffset: Option[Double] = None
  private var resizeBox: Option[Rectangle] = None
  target.onMousePressed = e => {
    if (e.isPrimaryButtonDown && e.getX > width.doubleValue() - 5 / context.scaled.doubleValue()) {
      resizeOffset = Some(width.doubleValue() - e.getX)
      val r = new Rectangle {
        x = 0.0
        y = -10.0
        height = 20.0
        width = self.width.doubleValue()
        fill = Color.DarkBlue.opacity(0.5)
      }
      children += r
      resizeBox = Some(r)
      e.consume()
    } else if (e.isPrimaryButtonDown && e.getX > width.doubleValue() - 8 / context.scaled.doubleValue()) {
      e.consume()
    }
  }
  target.onMouseReleased = e => {
    if (resizeOffset.isDefined) {
      val tmp = position()._1
      val newDuration = minDuration.max((e.getX + resizeOffset.get).max(0.05) / pxPerSec())

      val snapped = context.snap(tmp + newDuration, 0.0, false)

      if (context.content.intersecting(tmp, snapped).forall(p => tmp <= p._1 && p._2 <= position()._2)) {
        position = (tmp, snapped)
      }
      resizeBox.foreach(children.remove)
      e.consume()
    }
    resizeOffset = None
    if (e.getX < width.doubleValue() - 5 / context.scaled.doubleValue() || !contains(e.getX, e.getY)) {
      self.setCursor(Cursor.DEFAULT)
    }
  }
}


final class BeatView[B >: Beat <: TrackElement](val beat: Beat, val editable: Boolean, context: SelectionContainer[B]) extends TrackElementView[Beat, B](beat, context) {
  self =>

  override def position_=(value: (Double, Double)): Unit = {
    super.position_=(value._1, value._1 + 0.05)
  }

  val highlight = BooleanProperty(false)
  children = Seq(new Rectangle {
    width <== self.width
    height = 10
    y = -5
    if (editable) {
      fill <== when(selected).choose(Color.DarkBlue).otherwise(when(highlight || beat.highlight).choose(Color.OrangeRed).otherwise(Color.DarkRed))
    } else {
      fill <== when(highlight || beat.highlight).choose(Color.Black).otherwise(Color.DarkGray)
    }
  })
  val contextMenu = new ContextMenu(
    new MenuItem("Copy") {
      onAction = handle {
        context.copy(self)
      }
    },
    new MenuItem("Delete") {
      onAction = handle {
        context.delete(self)
      }
    },
    new CheckMenuItem("Highlight") {
      selected <==> beat.highlight
    }
  )
  onMouseClicked = e => {
    if (editable && MouseButton.Secondary.equals(e.getButton)) {
      contextMenu.show(self, Side.Bottom, e.getX * context.scaled.doubleValue(), e.getY - layoutBounds().getHeight)
      e.consume()
    } else {
      contextMenu.hide()
    }
  }
}

final class BeatsPatternView[B >: BeatsPattern <: TrackElement](val beatsPattern: BeatsPattern, context: SelectionContainer[B]) extends ResizableElementView[BeatsPattern, B](beatsPattern, context) with SelectionContainer[Beat] {
  self =>
  val content = beatsPattern.pattern
  val clazz = classOf[ImmutableBeat]

  override val selectionContainer = context.selectionContainer

  override val undoManager = context.undoManager

  override def scaled = context.scaled

  override def origin = -26

  override def snap(pos: Double, offset: Double, atStart: Boolean) = context.snap(pos, offset, atStart)

  val beatsGroup = new Group
  val element2view = mutable.Map[(Double, Beat), TrackElementView[Beat, Beat]]()

  override def getView(key: (Double, Beat)): TrackElementView[Beat, Beat] = element2view(key)

  override def startPosition = position()._1

  override def propagateDragOver(): Unit = {
    context.dragExit()
  }

  val scaleRect = new Rectangle {
    width <== when(self.width < 5) choose 5 otherwise self.width
    height = 20
    y = -10
    fill = Color.Transparent
  }
  lazy val rectangle = new Rectangle {
    width <== when(self.width < 5) choose 5 otherwise self.width
    height = 20
    y = -10
    fill <== when(selected).choose(Color.Blue).otherwise(Color.LightPink)
  }

  override def target = rectangle

  children = Seq(
    rectangle,
    beatsGroup,
    new Line {
      self2 =>
      startX <== beatsPattern.patternDuration * pxPerSec
      endX <== beatsPattern.patternDuration * pxPerSec
      startY = -10.0
      endY = 10.0
      strokeWidth = 0.5

      private var line: Option[Line] = None
      onMousePressed = e => {
        val l = new Line {
          startY = -10.0
          endY = 10.0
          strokeWidth = 0.5
          startX = beatsPattern.patternDuration() * pxPerSec()
          endX = beatsPattern.patternDuration() * pxPerSec()
          stroke = Color.DarkBlue.opacity(0.5)
        }
        self.children += l
        line = Some(l)
        e.consume()
      }
      onMouseMoved = e => {
        self2.delegate.setCursor(Cursor.H_RESIZE)
      }
      onMouseDragged = e => {
        val tmp = snap(0.05.max(e.getX / pxPerSec()), position()._1, true)
        line.get.startX = tmp * pxPerSec()
        line.get.endX = tmp * pxPerSec()
        if (e.isControlDown) {
          if (beatsPattern.pattern.canMove((0, beatsPattern.patternDuration()), (0, tmp))) {
            line.get.stroke = Color.DarkBlue
          } else {
            line.get.stroke = Color.DarkRed
          }
        } else if (tmp >= beatsPattern.pattern.lastOption.map(_._2).getOrElse(0.0)) {
          line.get.stroke = Color.DarkBlue
        } else {
          line.get.stroke = Color.DarkRed
        }
        e.consume()
      }
      onMouseReleased = e => {
        line.foreach(self.children.remove)
        line = None
        val tmp = snap(0.05.max(e.getX / pxPerSec()), position()._1, true)
        if (e.isControlDown) {
          if (beatsPattern.pattern.canMove((0, beatsPattern.patternDuration()), (0, tmp))) {
            context.undoManager.startGroup()
            beatsPattern.pattern.move((0, beatsPattern.patternDuration()), (0, tmp))
            beatsPattern.patternDuration() = beatsPattern.pattern.lastOption.map(_._2).getOrElse(0.0).max(tmp)
            context.undoManager.endGroup()
          }
        } else {
          beatsPattern.patternDuration() = beatsPattern.pattern.lastOption.map(_._2).getOrElse(0.0).max(tmp)
        }
        self2.delegate.setCursor(Cursor.DEFAULT)
        e.consume()
      }
    }
  )

  private def update(): Unit = {
    element2view.clear()
    if (beatsPattern.pattern.nonEmpty) {
      beatsGroup.children = beatsPattern.pattern.headOption.map { elem =>
        val v = new BeatView[Beat](elem._3, true, self)
        v.position = (elem._1, elem._2)
        v.pxPerSec <== pxPerSec
        v.layoutX <== pxPerSec * elem._1
        element2view.put((elem._1, elem._3), v)
        v.highlight <== beatsPattern.highlightFirst || beatsPattern.highlightFirstPattern
        v
      }.toIterable ++ (for ((start, end, b) <- beatsPattern.pattern.tail) yield {
        val v = new BeatView[Beat](b, true, self)
        v.position = (start, end)
        v.pxPerSec <== pxPerSec
        v.layoutX <== pxPerSec * start
        element2view.put((start, b), v)

        v.highlight <== beatsPattern.highlightFirstPattern
        v
      }) ++ (for ((start, end, b) <- beatsPattern.beats().drop(beatsPattern.pattern.size).takeWhile(_._2 < duration.get())) yield {
        val v = new BeatView[Beat](b, false, self)
        v.position = (start, end)
        v.pxPerSec <== pxPerSec
        v.layoutX <== pxPerSec * start
        element2view.put((start, b), v)
        v
      })
    } else {
      beatsGroup.children.clear()
    }
  }

  update()
  private val handler: InvalidationListener = _ => update()
  duration.addListener(weak(handler))
  beatsPattern.beats.addListener(weak(handler))

  override def minDuration = beatsPattern.patternDuration()

  override def selectionActive(x: => Double) = x / pxPerSec() <= beatsPattern.patternDuration()


  private var contextMenuX = 0.0
  val contextMenu = new ContextMenu(
    new MenuItem("New Beat") {
      onAction = handle {
        val b = new Beat(Some(context.undoManager))
        if (content.intersecting(contextMenuX, contextMenuX + 0.05).isEmpty) {
          content ++= Iterator((contextMenuX, contextMenuX + 0.05, b))
        }
      }
    },
    new MenuItem("Copy") {
      onAction = handle {
        context.copy(self)
      }
    },
    new MenuItem("Insert") {
      onAction = handle {
        insert(contextMenuX)
      }
    },
    new MenuItem("Delete") {
      onAction = handle {
        context.delete(self)
      }
    },
    new CheckMenuItem("Highlight First") {
      selected <==> beatsPattern.highlightFirst
    },
    new CheckMenuItem("Highlight Pattern") {
      selected <==> beatsPattern.highlightFirstPattern
    }
  )

  override def mouseClicked(e: MouseEvent) = {
    if (!e.isConsumed && MouseButton.Secondary.equals(e.getButton)) {
      contextMenuX = e.getX / pxPerSec()
      contextMenu.show(self, Side.Bottom, e.getX * context.scaled.doubleValue(), e.getY - layoutBounds().getHeight)
      e.consume()
      true
    } else {
      contextMenu.hide()
      false
    }
  }
}

final class BPMPatternView(val bpmPattern: BPMPattern, context: SelectionContainer[TrackElement], audio: ObjectProperty[Option[Array[Short]]]) extends ResizableElementView[BPMPattern, TrackElement](bpmPattern, context) {
  self =>
  override def target = this

  private val binding = when(width < 5) choose 5 otherwise width
  val beatsGroup = new Group
  children = Seq(
    new Rectangle {
      width <== binding
      height = 20
      y = -10
      fill <== when(selected).choose(Color.Blue).otherwise(Color.LightPink)
    },
    beatsGroup
  )

  private def update(): Unit = {
    beatsGroup.children = for ((start, end, b) <- bpmPattern.beats().takeWhile(_._2 < duration.get())) yield {
      val v = new BeatView[TrackElement](b, false, context)
      v.pxPerSec <== pxPerSec
      v.position = (start, end)
      v.layoutX <== pxPerSec * start
      v
    }
  }

  update()
  private val handler: InvalidationListener = _ => update()
  duration.addListener(weak(handler))
  bpmPattern.beats.addListener(weak(handler))

  val bpmSpinner = UIUtils.editableSpinner(new Spinner[Double](1.0, 600.0, bpmPattern.bpm(), 1.0))
  bpmSpinner.value.onChange { (_, old, now) =>
    if (old != now) {
      bpmPattern.bpm() = now
    }
  }
  bpmPattern.bpm.onChange { (_, old, now) =>
    if (old.doubleValue() != now.doubleValue()) {
      bpmSpinner.valueFactory().value = now.doubleValue()
    }
  }
  val contextMenu = new ContextMenu(
    new CustomMenuItem(bpmSpinner, false),
    new MenuItem("Copy") {
      onAction = handle {
        context.copy(self)
      }
    },
    new MenuItem("Delete") {
      onAction = handle {
        context.delete(self)
      }
    },
    new CheckMenuItem("Highlight First") {
      selected <==> bpmPattern.highlightFirst
    },
    new MenuItem("Detect BPM") {
      onAction = handle {
        audio().foreach { data =>
          val start = position()._1
          val end = position()._2
          val task = (callback: (Option[Double], Option[String]) => Boolean) => {
            val detection = new WaveletBPMDetection(WaveletBPMDetection.Daubechies4, 17)
            val dataSlice = data.slice((start * AudioPlayer.format.getFrameRate * 2).toInt, (end * AudioPlayer.format.getFrameRate * 2).toInt + 1)
            val monoData = dataSlice.toIterator.grouped(AudioPlayer.format.getChannels).map(_.map(_.toDouble).sum / Short.MaxValue).toArray
            val (bpm, bpms) = detection.detect(monoData, AudioPlayer.format.getSampleRate, Some((t, d) => {
              callback(Some(t / (end - start)), Some(f"$d%.3f bpm after $t%.3f"))
            }))
            success(Some(bpm))
          }
          val pd = new ProgressDialog[Double](Some(scene().windowProperty()()), "Detecting BPM", Some("preparing..."), true, task)
          pd.showAndWait().get.asInstanceOf[WithFailures[Option[Double], Throwable]] match {
            case WithFailures(Some(Some(bpm)), _) =>
              bpmPattern.bpm() = bpm
            case WithFailures(Some(None), _) =>
            case WithFailures(None, e) =>
              alert("Error during beat detection", e.headOption.map(x => x.getLocalizedMessage).getOrElse(""), scene().windowProperty()())
          }
        }

      }
    }
  )
  onMouseClicked = e => {
    if (MouseButton.Secondary.equals(e.getButton)) {
      contextMenu.show(self, Side.Bottom, e.getX * context.scaled.doubleValue(), e.getY - layoutBounds().getHeight)
      e.consume()
    } else {
      contextMenu.hide()
    }
  }
}

final class MessageView[B >: Message <: TrackElement](val message: Message, context: SelectionContainer[B]) extends ResizableElementView[Message, B](message, context) {
  self =>
  override def target = this

  children = Seq(
    new Rectangle {
      width <== when(self.width < 5) choose 5 otherwise self.width
      height = 20
      y = -10
      fill <== when(selected).choose(Color.Blue).otherwise(Color.LightGreen)
    },
    new Text {
      x = 3.0
      y = 0.0
      clip = new Rectangle {
        y = -8
        width <== when(self.width < 5) choose 5 otherwise self.width
        height = 16
      }
      text <== message.text
      textOrigin = VPos.CENTER
    }
  )
  val contextMenu = new ContextMenu(
    new CustomMenuItem(new TextField() {
      text <==> message.text
      editable = true

    }, false),
    new MenuItem("Copy") {
      onAction = handle {
        context.copy(self)
      }
    },
    new MenuItem("Delete") {
      onAction = handle {
        context.delete(self)
      }
    }
  )
  onMouseClicked = e => {
    if (MouseButton.Secondary.equals(e.getButton)) {
      contextMenu.show(self, Side.Bottom, e.getX * context.scaled.doubleValue(), e.getY - layoutBounds().getHeight)
      e.consume()
    } else {
      contextMenu.hide()
    }
  }
}