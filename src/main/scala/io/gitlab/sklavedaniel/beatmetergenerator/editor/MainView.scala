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

import io.gitlab.sklavedaniel.beatmetergenerator.utils._
import javafx.scene.layout
import scalafx.Includes._
import scalafx.beans.binding.{Bindings, ObjectBinding}
import scalafx.beans.property.DoubleProperty
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Group
import scalafx.scene.input.ScrollEvent
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.stage.Window

class MainView(player: AudioPlayer, val tracks: Tracks, digitDown: ObjectBinding[Option[Int]]) extends GridPane {
  main =>

  val undoManager = tracks.undoManager.get
  player.audio <== Bindings.createObjectBinding(() => tracks.audio().map(_._2), tracks.audio)
  player.beats.clear()

  val scale = DoubleProperty(1.0)
  val pxPerSec = scale * 20

  val headerBox = new VBox {
    spacing = 5
  }

  val tracksView = new TracksView(tracks, undoManager, player, digitDown, headerBox) {
    scale <== main.scale
    pxPerSec <== main.pxPerSec
  }

  filterEvent(ScrollEvent.Scroll) { (e: ScrollEvent) =>
    e.consume()
    if (e.isControlDown) {
      val x = tracksView.sceneToLocal(e.getSceneX, e.getSceneY).getX
      val oldPosition = (tracksView.scrollX + x) / scale()
      scale() = (scale() * (1 + e.getDeltaY / 400)).max(0.25).min(40.0)
      tracksView.scrollX = oldPosition * scale() - x
    } else if (e.isShiftDown) {
      tracksView.scrollY -= e.getDeltaX()
    } else {
      tracksView.scrollX += e.getDeltaY()
    }
  }

  player.position.onChange { (_, _, v) =>
    if (v._1 * pxPerSec.doubleValue() < tracksView.scrollX + 0.1 * tracksView.viewportBounds().getWidth) {
      tracksView.scrollX = v._1 * pxPerSec.doubleValue() - 0.1 * tracksView.viewportBounds().getWidth
    } else if (v._1 * pxPerSec.doubleValue() > tracksView.scrollX + 0.9 * tracksView.viewportBounds().getWidth) {
      tracksView.scrollX = v._1 * pxPerSec.doubleValue() - 0.9 * tracksView.viewportBounds().getWidth
    }
  }

  val waveHeight = 200

  val waveView = new WaveView(player.position) {
    points <== player.maxima
    duration <== tracksView.totalDuration
    waveDuration <== player.audioDuration
    pxPerSec <== main.pxPerSec
    visiblePosition <== tracksView.visiblePosition
    padding = Insets(1.0)
    prefHeight = waveHeight
    minHeight = waveHeight
    maxHeight = waveHeight
    delegate.setBorder(new layout.Border(new layout.BorderStroke(Color.Gray, BorderStrokeStyle.Solid.delegate, CornerRadii.Empty.delegate, BorderWidths.Default.delegate)))
  }

  val positionOverlay = new PositionOverlay {
    duration <== tracksView.totalDuration
    pxPerSec <== main.pxPerSec
    position <== Bindings.createDoubleBinding(() => player.position()._1, player.position)
    visiblePosition <== tracksView.visiblePosition
    padding = Insets(1.0)
    val h = Bindings.createDoubleBinding(() => tracksView.viewportBounds().getHeight + waveHeight + 3, tracksView.viewportBounds)
    lineHeight <== h
  }

  val headerGroup = new Pane {
    self =>
    val box = new Group {
      layoutY <== Bindings.createDoubleBinding(() => -tracksView.scrollY, tracksView.vvalue, tracksView.viewportBounds, tracksView.content().boundsInLocal)
      children = Seq(new VBox {
        spacing = 5
        children = Seq(
          new Rectangle {
            height = 203
            width = 100
            fill = Color.Transparent
          },
          headerBox
        )
      })
    }
    minWidth <== Bindings.createDoubleBinding(() => box.layoutBounds().getWidth, box.layoutBounds)
    children = Seq(box)
    clip = new Rectangle {
      width <== Bindings.createDoubleBinding(() => self.layoutBounds().getWidth, self.layoutBounds)
      height <== Bindings.createDoubleBinding(() => self.layoutBounds().getHeight, self.layoutBounds)
    }
  }

  add(headerGroup, 0, 0)
  add(new StackPane {
    alignment = Pos.TopCenter
    hgrow = Priority.Always
    vgrow = Priority.Always

    children = Seq(
      new VBox {
        spacing = 1
        children = Seq(waveView, tracksView)
      },
      positionOverlay
    )
  }, 1, 0)

  columnConstraints = Seq(new ColumnConstraints(), new ColumnConstraints {
    hgrow = Priority.Always
    maxWidth = Double.PositiveInfinity
    fillWidth = true
  })
  rowConstraints = Seq(new RowConstraints() {
    vgrow = Priority.Always
    maxHeight = Double.PositiveInfinity
    fillHeight = true
  })
}