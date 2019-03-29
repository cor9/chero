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


import javafx.scene.Cursor
import scalafx.Includes._
import scalafx.animation._
import scalafx.beans.binding.Bindings
import scalafx.beans.property._
import scalafx.scene.Group
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.util.Duration


class PositionLineView(initPxPerSec: Double, initWidth: Double, sceeneToContextX: Double => Double) extends Group {
  self =>
  val position = ObjectProperty((0.0, true))
  val pxPerSec = DoubleProperty(initPxPerSec)
  val scale = DoubleProperty(1.0)
  val height = DoubleProperty(0.0)
  val width = DoubleProperty(initWidth)
  val duration = DoubleProperty(0.0)
  private val pxDuration_ = ReadOnlyDoubleWrapper(0.0)
  pxDuration_ <== pxPerSec * scale * duration
  val pxDuration = pxDuration_.getReadOnlyProperty
  private val pxPosition_ = ReadOnlyDoubleWrapper(0.0)
  pxPosition_ <== pxPerSec * scale * Bindings.createDoubleBinding(() => position()._1, position)
  val pxPosition = pxPosition_.readOnlyProperty

  children = Seq(
    new Rectangle {
      x <== when(pxPosition < self.width / 2) choose 0.0 otherwise pxPosition - self.width / 2
      y = 0
      width <== when(pxPosition < self.width / 2) choose pxPosition + self.width / 2 otherwise (
        when(pxDuration - pxPosition < self.width / 2) choose pxDuration - pxPosition + self.width / 2 otherwise self.width)
      height <== self.height
      fill = Color.Transparent
    },
    new Line {
      startX <== pxPosition
      startY = 0
      endX <== startX
      endY <== height
      stroke = Color.Red
    }
  )

  private val dragging_ = ReadOnlyBooleanWrapper(false)
  val dragging = dragging_.getReadOnlyProperty

  onMouseEntered = e => {
    if (!dragging_()) {
      self.setCursor(Cursor.HAND)
    }
  }
  onMousePressed = e => {
    if (e.isPrimaryButtonDown) {
      self.setCursor(Cursor.MOVE)
      dragging_() = true
    }
    e.consume()
  }
  onMouseReleased = e => {
    self.setCursor(Cursor.HAND)
    dragging_() = false
    e.consume()
  }
  onMouseExited = e => {
    if (!dragging_()) {
      self.setCursor(Cursor.DEFAULT)
    }
  }
  private var dragTimeline: Option[Timeline] = None
  onMouseDragged = e => {
    def drag(): Unit = {
      if (dragging()) {
        val v = sceeneToContextX(e.getSceneX)
        if ((pxPosition() - v).abs >= 5.0) {
          position() = ((v / pxPerSec() / scale()).max(0.0).min(duration()), true)
          val tl = Timeline(KeyFrame(time = Duration(50), onFinished = _ => {
            drag()
          }))
          tl.play()
          dragTimeline = Some(tl)
        } else {
          position() = ((v / pxPerSec() / scale()).max(0.0).min(duration()), true)
        }
      }
    }

    dragTimeline.foreach(_.stop())
    dragTimeline = None

    drag()
  }


}