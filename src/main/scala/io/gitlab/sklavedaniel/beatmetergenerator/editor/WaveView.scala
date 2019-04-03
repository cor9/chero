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

import scalafx.Includes._
import scalafx.animation.{Animation, KeyFrame, Timeline}
import scalafx.beans.binding.Bindings
import scalafx.beans.property.{DoubleProperty, ObjectProperty}
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color
import scalafx.util.Duration

class WaveView(position: ObjectProperty[(Double, Boolean)]) extends Pane {
  self =>
  val pxPerSec = DoubleProperty(1.0)
  val points = ObjectProperty[Option[(Array[Float], Array[Float])]](None)
  val waveDuration = DoubleProperty(0.0)
  val duration = DoubleProperty(0.0)
  val visiblePosition = DoubleProperty(0.0)

  val realX = Bindings.createDoubleBinding(() => padding().getLeft, padding)
  val realWidth = Bindings.createDoubleBinding(() => width.value - padding().getLeft - padding().getRight, width, padding)
  val realHeight = Bindings.createDoubleBinding(() => height.value - padding().getTop - padding().getBottom, height, padding)
  val maxVolume = Bindings.createObjectBinding[Option[Float]](() => points().map(p => p._1.max max p._2.max), points)
  val visibleX = pxPerSec * visiblePosition

  val canvas = new Canvas {
    layoutX = 0.0
    layoutY = 0.0
    width <== realWidth
    height <== self.height
  }
  children = Seq(canvas)

  val wave = Bindings.createObjectBinding[Seq[(Double, Double, Double)]](() => {
    points().map { p =>
      val mv = maxVolume().get
      val rate = p._1.length / waveDuration.floatValue()
      val fromPos = visibleX.doubleValue().floor.toInt.max(0)
      val toPos = (visibleX.doubleValue().max(0.0) + realWidth.doubleValue()).ceil.toInt min (waveDuration() * pxPerSec.doubleValue()).floor.toInt
      (for (i <- fromPos until toPos) yield {
        val from = (i / pxPerSec.doubleValue() * rate).floor.toInt
        val to = ((i + 1) / pxPerSec.doubleValue() * rate).ceil.toInt
        val max1 = p._1.slice(from, to).max
        val max2 = p._2.slice(from, to).max
        (i + 0.5 - fromPos, self.height() / 2 * (1 + max2 / mv), self.height() / 2 * (1 - max1 / mv))
      }).toList
    }.getOrElse(Nil)
  }, points, waveDuration, realWidth, visibleX, pxPerSec)

  wave.onChange { (_, _, _) =>
    val gc = canvas.getGraphicsContext2D()
    gc.clearRect(0.0, 0.0, realWidth.doubleValue(), self.height())
    gc.setStroke(Color.Black)
    for ((x, y1, y2) <- wave()) {
      gc.strokeLine(x, y1, x, y2)
    }
  }

  val timeline = new Timeline {
    keyFrames = KeyFrame(time = Duration(50), onFinished = _ => {
      updateMouse()
    })
    cycleCount = Animation.Indefinite
  }

  private var mouseX: Option[Double] = None

  def updateMouse(): Unit = {
    mouseX.foreach { x =>
      val p = (visiblePosition() + (x - realX.doubleValue()) / pxPerSec.doubleValue()).max(0.0)
      position() = (p, true)
    }
  }

  onMousePressed = e => {
    timeline.stop()
    if (e.isPrimaryButtonDown ) {
      mouseX = Some(e.getX)
      updateMouse()
      timeline.play()
    }
  }

  onMouseDragged = e => {
    timeline.stop()
    mouseX = Some(e.getX)
    updateMouse()
    timeline.play()
  }

  onMouseReleased = e => {
    timeline.stop()
  }

  onMouseExited = e => {
    timeline.stop()
  }
}
