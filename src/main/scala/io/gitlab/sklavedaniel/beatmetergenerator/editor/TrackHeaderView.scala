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

import java.io.{BufferedInputStream, FileInputStream}
import java.net.URI

import io.gitlab.sklavedaniel.beatmetergenerator.editor.BeatEditor.{tracks, _}
import io.gitlab.sklavedaniel.beatmetergenerator.utils.{Track, Tracks, UndoManager, WithFailures}
import javafx.beans.value.ChangeListener
import javafx.geometry.VPos
import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty, ObjectProperty}
import scalafx.geometry.Side
import scalafx.scene.Group
import scalafx.scene.control._
import scalafx.scene.input.MouseButton
import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.{Font, Text}
import scalafx.stage.FileChooser
import scalafx.stage.FileChooser.ExtensionFilter
import Utils._

class TrackHeaderView(val track: Track, tracks: Tracks, undoManager: UndoManager) extends Group {
  self =>
  val width = DoubleProperty(0.0)

  val contextMenu = new ContextMenu(
    new CustomMenuItem(new TextField() {
      text <==> track.title
      editable = true

    }, false),
    new CheckMenuItem("Custom Sound") {
      val listener: ChangeListener[Option[(URI, Array[Short])]] = (_, _, b) => {
        selected() = b.isDefined
      }
      selected = track.beat().isDefined
      track.beat.addListener(weak(listener))
      selected.onChange { (_, _, current) =>
        if (current) {
          if (track.beat().isEmpty) {
            val fc = new FileChooser()
            tracks.file().foreach {f =>
              fc.initialDirectory = f.getParentFile
            }
            fc.title = "Beatmeter Generator: Open audio file"
            fc.getExtensionFilters += new ExtensionFilter("wav audio file (16bit unsigned)", "*.wav")
            Option(fc.showOpenDialog(null)) match {
              case Some(file) =>
                AudioPlayer.readData(new BufferedInputStream(new FileInputStream(file))) match {
                  case WithFailures(Some(data), _) =>
                    track.beat() = Some((file.toURI, data))
                  case WithFailures(None, e) =>
                    alert("Could not load file", e.head.getLocalizedMessage, scene().windowProperty()())
                    selected() = false
                }
              case None =>
                selected() = false
            }
          }
        } else if (track.beat().isDefined) {
          track.beat() = None
        }
      }
    },
    new MenuItem("Move up") {
      onAction = handle {
        if (tracks.content.head != track) {
          val index = tracks.content.indexOf(track)
          undoManager.startGroup()
          tracks.content.remove(track)
          tracks.content.add(index - 1, track)
          undoManager.endGroup()
        }
      }
    },
    new MenuItem("Move down") {
      onAction = handle {
        if (tracks.content.last != track) {
          val index = tracks.content.indexOf(track)
          undoManager.startGroup()
          tracks.content.remove(track)
          tracks.content.add(index + 1, track)
          undoManager.endGroup()
        }
      }
    },
    new MenuItem("Delete") {
      onAction = handle {
        tracks.content.remove(track)
      }
    }
  )
  onMouseClicked = e => {
    if (MouseButton.Secondary.equals(e.getButton)) {
      contextMenu.show(self, Side.Bottom, e.getX, e.getY - layoutBounds().getHeight)
    } else {
      contextMenu.hide()
    }
  }
  children = Seq(
    new Rectangle {
      x <== 0.0
      y = 0.0
      height = 50.0
      width <== self.width
      fill = Color.LightGray
    },
    new Text {
      x = 2.0
      y = 2.0
      textOrigin = VPos.TOP
      text <== track.title
    },
    new HBox {
      layoutY = 30
      layoutX = 2.0
      spacing = 2
      children = Seq(
        new ToggleButton {
          text = "P"
          tooltip = new Tooltip("mute/unmute track") {
            font = Font(10)
          }
          font = Font(8)
          focusTraversable = false
          selected <==> track.play
        },
        new ToggleButton {
          text = "S"
          tooltip = new Tooltip("use track for snapping") {
            font = Font(10)
          }
          font = Font(8)
          focusTraversable = false
          selected = track.snap()
          selected.onChange { (_, _, current) =>
            if (track.snap() != current) {
              undoManager.startGroup()
              track.snap() = current
              undoManager.endGroup()
            }
          }
          val listener: ChangeListener[java.lang.Boolean] = (_, _, current) => {
            if (selected() != current) {
              selected() = current
            }
          }
          track.snap.addListener(weak(listener))
        },
        new ToggleButton {
          text = "I"
          tooltip = new Tooltip("insert into this track") {
            font = Font(10)
          }
          font = Font(8)
          focusTraversable = false
          selected = track.record()
          selected.onChange { (_, _, current) =>
            if (track.record() != current) {
              undoManager.startGroup()
              track.record() = current
              undoManager.endGroup()
            }
          }
          val listener: ChangeListener[java.lang.Boolean] = (_, _, current) => {
            if (selected() != current) {
              selected() = current
            }
          }
          track.record.addListener(weak(listener))
        },
        new ToggleButton {
          text = "D"
          tooltip = new Tooltip("display track on beatmeter") {
            font = Font(10)
          }
          font = Font(8)
          focusTraversable = false
          selected <==> track.display
        }
      )
    }
  )
}