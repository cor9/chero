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
import scalafx.beans.property.DoubleProperty
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color
import scalafx.scene.shape.Line

class PositionOverlay extends Pane {
  self =>
  val pxPerSec = DoubleProperty(1.0)
  val duration = DoubleProperty(0.0)
  val position = DoubleProperty(0.0)
  val visiblePosition = DoubleProperty(0.0)
  val lineHeight = DoubleProperty(0.0)

  val realWidth = Bindings.createDoubleBinding(() => width.value - padding().getLeft - padding().getRight, width, padding)
  val realHeight = Bindings.createDoubleBinding(() => lineHeight.value - padding().getTop - padding().getBottom, lineHeight, padding)
  val realX = Bindings.createDoubleBinding(() => padding().getLeft, padding)
  val realY = Bindings.createDoubleBinding(() => padding().getTop, padding)

  val visibleDuration = realWidth / pxPerSec

  val lineVisible = position >= visiblePosition && position <= visiblePosition + visibleDuration
  val lineX = when(lineVisible) choose pxPerSec * (position - visiblePosition) otherwise 0.0

  mouseTransparent = true

  children = Seq(new Line {
    startX <== lineX + realX
    startY = 0.0
    endX <== lineX + realX
    endY <== realHeight
    stroke = Color.DarkRed
    visible <== lineVisible
    strokeWidth = 1.0
  })
}
