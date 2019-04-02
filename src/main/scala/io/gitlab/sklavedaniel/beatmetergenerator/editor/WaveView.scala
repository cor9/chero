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

import io.gitlab.sklavedaniel.beatmetergenerator.editor.Utils._
import scalafx.Includes._
import scalafx.beans.binding.{Bindings, NumberBinding}
import scalafx.beans.property.{DoubleProperty, ObjectProperty}
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout.Pane

class WaveView(sceneToContextX: Double => Double) extends Pane {
  self =>
  val pxPerSec = DoubleProperty(1.0)
  val hvalue = DoubleProperty(0.0)
  val visibleWidth = DoubleProperty(1.0)
  val points = ObjectProperty[Option[(Array[Float], Array[Float])]](None)
  val duration = DoubleProperty(0.0)

  val maxVolume = Bindings.createObjectBinding[Option[Float]](() => points().map(p => p._1.max max p._2.max), points)
  val visibleTime = hvalue * (duration - visibleWidth / pxPerSec)
  val visiblePos = hvalue * (pxPerSec * duration - visibleWidth)
  val visibleDuration = width / pxPerSec

  val canvas = new Canvas {
    layoutX = 0.0
    layoutY = 0.0
    width <== self.width
    height <== self.height
  }
  children = Seq(canvas)

  val wave = Bindings.createObjectBinding[Seq[(Double, Double, Double)]](() => {
    points().map { p =>
      val mv = maxVolume().get
      val rate = p._1.length / duration.floatValue()
      val fromPos = visiblePos.doubleValue().floor.toInt.max(0)
      val toPos = (visiblePos.doubleValue().max(0.0) + width.doubleValue()).ceil.toInt min (duration() * pxPerSec.doubleValue()).ceil.toInt
      (for (i <- fromPos until toPos) yield {
        val from = (i / pxPerSec.doubleValue() * rate).floor.toInt
        val to = ((i + 1) / pxPerSec.doubleValue() * rate).ceil.toInt
        val max1 = p._1.slice(from, to).max
        val max2 = p._2.slice(from, to).max
        (i + 0.5 - fromPos, self.height() / 2 * (1 + max2 / mv), self.height() / 2 * (1 - max1 / mv))
      }).toList
    }.getOrElse(Nil)
  }, points, duration, width, visiblePos, hvalue)

  wave.onChange { (_, _, v) =>
    val gc = canvas.getGraphicsContext2D()
    gc.clearRect(0.0, 0.0, self.width(), self.height())
    for ((x,y1,y2) <- v) {
      gc.strokeLine(x,y1,x,y2)
    }
  }

  val onAction = ObjectProperty((p: Double) => ())

  onMouseClicked = mouseHandler(e => {
    onAction()(sceneToContextX(e.getSceneX) / pxPerSec.doubleValue())
  }, _ => ())
}
