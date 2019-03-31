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
import scalafx.beans.binding.Bindings
import scalafx.beans.property.{DoubleProperty, ObjectProperty}
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.scene.{Group, Node}
import Utils._

class WaveView(initPxPerSec: Double, initHeight: Double, sceneToContextX: Double => Double) extends Group {
  self =>
  val hvalue = DoubleProperty(0.0)
  val visibleWidth = DoubleProperty(1.0)
  val points = ObjectProperty[Option[(Array[Float], Array[Float])]](None)
  val scale = DoubleProperty(1.0)
  val pxPerSec = scale * initPxPerSec
  val maxVolume = Bindings.createObjectBinding[Option[Float]](() => points().map(p => p._1.max max p._2.max), points)
  val height = DoubleProperty(initHeight)
  val duration = DoubleProperty(0.0)
  val width = duration * pxPerSec

  val visibleTime = hvalue * (duration - visibleWidth / pxPerSec)
  val visiblePos = hvalue * (pxPerSec * duration - visibleWidth)
  val visibleDuration = visibleWidth / pxPerSec

  val rect = new Rectangle {
    x = 0.0
    y = 0.0
    width <== pxPerSec * duration
    height <== self.height
    fill = Color.Transparent
  }
  val pane = new Group {

  }
  children = Seq(rect, pane)

  val wave = Bindings.createObjectBinding[Seq[Node]](() => {
    points().map { p =>
      val mv = maxVolume().get

      val rate = p._1.length / duration.floatValue()
      val fromPos = visiblePos.doubleValue().floor.toInt.max(0)
      val toPos = (visiblePos.doubleValue().max(0.0) + visibleWidth.doubleValue()).ceil.toInt min width.doubleValue().ceil.toInt
      (for (i <- fromPos until toPos) yield {
        val from = (i / pxPerSec.doubleValue() * rate).floor.toInt
        val to = ((i + 1) / pxPerSec.doubleValue() * rate).ceil.toInt
        val max1 = p._1.slice(from, to).max
        val max2 = p._2.slice(from, to).max
        new Line {
          startX = i + 0.5
          endX = i + 0.5
          startY <== self.height / 2 * (1 + max2 / mv)
          endY <== self.height / 2 * (1 - max1 / mv)
        }
      }).toList
    }.getOrElse(Nil)
  }, points, duration, visibleWidth, hvalue, scale)

  wave.onChange { (_, _, v) =>
    pane.children = v
  }

  val onAction = ObjectProperty((p: Double) => ())

  onMouseClicked = mouseHandler(e => {
    onAction()(sceneToContextX(e.getSceneX) / pxPerSec.doubleValue())
  }, _ => ())
}
