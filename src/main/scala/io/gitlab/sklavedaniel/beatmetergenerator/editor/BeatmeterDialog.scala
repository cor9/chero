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

import io.gitlab.sklavedaniel.beatmetergenerator.beatmeters.FlyingBeatmeter2
import io.gitlab.sklavedaniel.beatmetergenerator.beatmeters.WaveformBeatmeter2

import scalafx.Includes._
import scalafx.beans.binding.Bindings
import scalafx.beans.property._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.stage.{DirectoryChooser, Window}

class BeatmeterDialog(ownerWindow: Option[Window], flying: Boolean, flyingConf: FlyingBeatmeter2.Conf, waveformConf: WaveformBeatmeter2.Conf)
  extends Dialog[(Boolean, FlyingBeatmeter2.Conf, WaveformBeatmeter2.Conf)] {
  ownerWindow.foreach(initOwner)
  title = "Beatmeter Generator"

  object Flying {
    val widthSpinner = new Spinner[Int](1, 10000, flyingConf.width, 1) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val heightSpinner = new Spinner[Int](1, 10000, flyingConf.height, 1) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val framesSpinner = new Spinner[Double](1, 240, flyingConf.frames, 0.1) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val speedSpinner = new Spinner[Double](0.1, 1.0, flyingConf.speed, 0.05) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val positionSpinner = new Spinner[Double](0.0, 1.0, flyingConf.position, 0.05) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val startPositionSpinner = new Spinner[Double](0.0, 1.0, flyingConf.position, 0.05) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }

    val beatColorPicker = new ColorPicker(flyingConf.beatColor) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val beatBorderColorPicker = new ColorPicker(flyingConf.beatBorderColor) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }

    val beatHighlightColorPicker = new ColorPicker(flyingConf.beatHighlightedColor) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val beatHighlightBorderColorPicker = new ColorPicker(flyingConf.beatHighlightedBorderColor) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val messageFont = ObjectProperty(flyingConf.messageFont)
    val marginSpinner = new Spinner[Int](0, 10000, flyingConf.margin, 1) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val messageColorPicker = new ColorPicker(flyingConf.messageColor) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val messageBorderColorPicker = new ColorPicker(flyingConf.messageBorderColor) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val messageBorderStrengthSpinner = new Spinner[Double](0.0, 10.0, flyingConf.messagePosition, 0.5) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val messageAlign = new ComboBox[String](Seq("Left", "Center", "Right")) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      selectionModel().select(flyingConf.messageAlign match {
        case AlignLeft => 0
        case AlignCenter => 1
        case AlignRight => 2
      })
    }
    val messagePositionSpinner = new Spinner[Double](0.0, 1.0, flyingConf.messagePosition, 0.05) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val imageDirectory = ObjectProperty(flyingConf.imageDirectory)
    val pane = new GridPane {
      hgap = 10
      vgap = 5
      padding = Insets(10, 10, 10, 10)
      add(new Label("Width") {
        tooltip = Tooltip("Width of the generated video in px")
      }, 0, 0)
      add(widthSpinner, 1, 0)
      add(new Label("Framerate") {
        tooltip = Tooltip("Framerate in frames per sec")
      }, 0, 1)
      add(framesSpinner, 1, 1)
      add(new Label("Beatmeter Height") {
        tooltip = Tooltip("Height of the beatmeter in px")
      }, 0, 2)
      add(heightSpinner, 1, 2)
      add(new Label("Position") {
        tooltip = Tooltip("Position of beat when it is played as ratio of video width")
      }, 0, 3)
      add(positionSpinner, 1, 3)
      add(new Label("Speed") {
        tooltip = Tooltip("Speed of beats as ratio of video width")
      }, 0, 4)
      add(speedSpinner, 1, 4)
      add(new Label("Beat Color"), 0, 5)
      add(beatColorPicker, 1, 5)
      add(new Label("Beat Border Color"), 0, 6)
      add(beatBorderColorPicker, 1, 6)
      add(new Label("Beat Highlight Color"), 0, 7)
      add(beatHighlightColorPicker, 1, 7)
      add(new Label("Beat Highlight Border Color"), 0, 8)
      add(beatHighlightBorderColorPicker, 1, 8)
      add(new Label("Beat Message Margin") {
        tooltip = Tooltip("Margin between beatmeter and messages in px")
      }, 0, 10)
      add(marginSpinner, 1, 10)
      add(new Label("Message Text Color"), 0, 11)
      add(messageColorPicker, 1, 11)
      add(new Label("Message Border Color"), 0, 12)
      add(messageBorderColorPicker, 1, 12)
      add(new Label("Message Border Strength"), 0, 13)
      add(messageBorderStrengthSpinner, 1, 13)
      add(new Label("Message Font"), 0, 14)
      add(new HBox(
        new TextField {
          editable = false
          text <== Bindings.createStringBinding(() => {
            val (family, size, bold, italic) = messageFont()
            family + " " + size + (if (bold) {
              " bold"
            } else {
              ""
            }) + (if (italic) {
              " italic"
            } else {
              ""
            })
          }, messageFont)
        },
        new Button("Choose") {
          onAction = handle {
            val fsd = new FontDialog(Some(scene().windowProperty().get()), messageFont())
            messageFont() = fsd.showAndWait().get.asInstanceOf[(String, Int, Boolean, Boolean)]
          }
        },
      ) {
        spacing = 5
      }, 1, 14)
      add(new Label("Message Alignment"), 0, 15)
      add(messageAlign, 1, 15)
      add(new Label("Message Position") {
        tooltip = Tooltip("Horizontal position the message is aligned to as ratio of video width")
      }, 0, 16)
      add(messagePositionSpinner, 1, 16)
      add(new Label("Image Directory") {
        tooltip = Tooltip("Directory containing beat.svg and beat.anim.")
      }, 0, 17)
      add(new HBox(
        new ToggleButton("Select") {
          imageDirectory.onChange { (_, _, d) =>
            selected() = d.isDefined
          }
          selected.onChange { (_, old, current) =>
            if (!old && current) {
              val dialog = new DirectoryChooser()
              dialog.title = "Beatmeter Generator: Image Directory"
              imageDirectory() = Option(dialog.showDialog(scene().windowProperty()())).filter(_.exists()).map(_.toURI)
            } else if (old && !current) {
              imageDirectory() = None
            }
          }
        }, new TextField() {
          hgrow = Priority.Always
          maxWidth = Double.PositiveInfinity
          editable = false
          text <== Bindings.createStringBinding(() => imageDirectory().map(_.toString).getOrElse(""), imageDirectory)
        }
      ) {
        spacing = 5
      }, 1, 17)
    }

    def getConf() = {
      FlyingBeatmeter2.Conf(widthSpinner.value(), heightSpinner.value(), framesSpinner.value(), speedSpinner.value(),
        positionSpinner.value(), beatColorPicker.value(), beatBorderColorPicker.value(), beatHighlightColorPicker.value(),
        beatHighlightBorderColorPicker.value(), messageFont(), marginSpinner.value(),
        messageColorPicker.value(), messageBorderColorPicker.value(), messageBorderStrengthSpinner.value(),
        messageAlign.selectionModel().getSelectedIndex match {
          case 0 => AlignLeft
          case 1 => AlignCenter
          case 2 => AlignRight
        }, messagePositionSpinner.value(), imageDirectory())
    }
  }

  object Waveform {
    val widthSpinner = new Spinner[Int](1, 10000, waveformConf.width, 1) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val heightSpinner = new Spinner[Int](1, 10000, waveformConf.height, 1) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val framesSpinner = new Spinner[Double](1, 240, waveformConf.frames, 0.1) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val speedSpinner = new Spinner[Double](0.1, 1.0, waveformConf.speed, 0.05) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val positionSpinner = new Spinner[Double](0.0, 1.0, waveformConf.position, 0.05) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val startPositionSpinner = new Spinner[Double](0.0, 1.0, waveformConf.startPosition, 0.05) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val endPositionSpinner = new Spinner[Double](0.0, 1.0, waveformConf.endPosition, 0.05) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }

    val waveColorPicker = new ColorPicker(waveformConf.waveColor) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val waveHighlightColorPicker = new ColorPicker(waveformConf.waveHighlightedColor) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val foregroundColorPicker = new ColorPicker(waveformConf.foregroundColor) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val backgroundColorPicker = new ColorPicker(waveformConf.backgroundColor) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val markerColorPicker = new ColorPicker(waveformConf.markerColor) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val messageFont = ObjectProperty(waveformConf.messageFont)
    val marginSpinner = new Spinner[Int](0, 10000, waveformConf.margin, 1) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val messageColorPicker = new ColorPicker(waveformConf.messageColor) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val messageBorderColorPicker = new ColorPicker(waveformConf.messageBorderColor) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val messageBorderStrengthSpinner = new Spinner[Double](0.0, 10.0, waveformConf.messagePosition, 0.5) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val messageAlign = new ComboBox[String](Seq("Left", "Center", "Right")) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      selectionModel().select(waveformConf.messageAlign match {
        case AlignLeft => 0
        case AlignCenter => 1
        case AlignRight => 2
      })
    }
    val messagePositionSpinner = new Spinner[Double](0.0, 1.0, waveformConf.messagePosition, 0.05) {
      hgrow = Priority.Always
      maxWidth = Double.PositiveInfinity
      editable = true
    }
    val imageDirectory = ObjectProperty(waveformConf.imageDirectory)
    val pane = new GridPane {
      hgap = 10
      vgap = 5
      padding = Insets(10, 10, 10, 10)
      add(new Label("Width") {
        tooltip = Tooltip("Width of the generated video in px")
      }, 0, 0)
      add(widthSpinner, 1, 0)
      add(new Label("Framerate") {
        tooltip = Tooltip("Framerate in frames per sec")
      }, 0, 1)
      add(framesSpinner, 1, 1)
      add(new Label("Beatmeter Height") {
        tooltip = Tooltip("Height of the beatmeter in px")
      }, 0, 2)
      add(heightSpinner, 1, 2)
      add(new Label("Position") {
        tooltip = Tooltip("Position of beat when it is played as ratio of video width")
      }, 0, 3)
      add(positionSpinner, 1, 3)
      add(new Label("Left Position") {
        tooltip = Tooltip("Position where beats disappear as ratio of video width")
      }, 0, 4)
      add(endPositionSpinner, 1, 4)
      add(new Label("Right Position") {
        tooltip = Tooltip("Position where beats appear as ratio of video width")
      }, 0, 5)
      add(startPositionSpinner, 1, 5)
      add(new Label("Speed") {
        tooltip = Tooltip("Speed of beats as ratio of video width")
      }, 0, 6)
      add(speedSpinner, 1, 6)
      add(new Label("Wave Color"), 0, 7)
      add(waveColorPicker, 1, 7)
      add(new Label("Wave Higlighted Color"), 0, 8)
      add(waveHighlightColorPicker, 1, 8)
      add(new Label("Foreground Color"), 0, 9)
      add(foregroundColorPicker, 1, 9)
      add(new Label("Background Color"), 0, 10)
      add(backgroundColorPicker, 1, 10)
      add(new Label("Marker Color"), 0, 11)
      add(markerColorPicker, 1, 11)
      add(new Label("Beat Message Margin") {
        tooltip = Tooltip("Margin between beatmeter and messages in px")
      }, 0, 12)
      add(marginSpinner, 1, 12)
      add(new Label("Message Text Color"), 0, 13)
      add(messageColorPicker, 1, 13)
      add(new Label("Message Border Color"), 0, 14)
      add(messageBorderColorPicker, 1, 14)
      add(new Label("Message Border Strength"), 0, 15)
      add(messageBorderStrengthSpinner, 1, 15)
      add(new Label("Message Font"), 0, 16)
      add(new HBox(
        new TextField {
          editable = false
          text <== Bindings.createStringBinding(() => {
            val (family, size, bold, italic) = messageFont()
            family + " " + size + (if (bold) {
              " bold"
            } else {
              ""
            }) + (if (italic) {
              " italic"
            } else {
              ""
            })
          }, messageFont)
        },
        new Button("Choose") {
          onAction = handle {
            val fsd = new FontDialog(Some(scene().windowProperty().get()), messageFont())
            messageFont() = fsd.showAndWait().get.asInstanceOf[(String, Int, Boolean, Boolean)]
          }
        },
      ) {
        spacing = 5
      }, 1, 16)
      add(new Label("Message Alignment"), 0, 17)
      add(messageAlign, 1, 17)
      add(new Label("Message Position") {
        tooltip = Tooltip("Horizontal position the message is aligned to as ratio of video width")
      }, 0, 18)
      add(messagePositionSpinner, 1, 18)
      add(new Label("Image Directory") {
        tooltip = Tooltip("Directory containing beat.svg and beat.anim.")
      }, 0, 19)
      add(new HBox(
        new ToggleButton("Select") {
          imageDirectory.onChange { (_, _, d) =>
            selected() = d.isDefined
          }
          selected.onChange { (_, old, current) =>
            if (!old && current) {
              val dialog = new DirectoryChooser()
              dialog.title = "Beatmeter Generator: Image Directory"
              imageDirectory() = Option(dialog.showDialog(scene().windowProperty()())).filter(_.exists()).map(_.toURI)
            } else if (old && !current) {
              imageDirectory() = None
            }
          }
        }, new TextField() {
          hgrow = Priority.Always
          maxWidth = Double.PositiveInfinity
          editable = false
          text <== Bindings.createStringBinding(() => imageDirectory().map(_.toString).getOrElse(""), imageDirectory)
        }
      ) {
        spacing = 5
      }, 1, 19)
    }

    def getConf() = {
      WaveformBeatmeter2.Conf(widthSpinner.value(), heightSpinner.value(), framesSpinner.value(), speedSpinner.value(),
        positionSpinner.value(), startPositionSpinner.value(), endPositionSpinner.value(), waveColorPicker.value(),
        waveHighlightColorPicker.value(), foregroundColorPicker.value(),
        backgroundColorPicker.value(), markerColorPicker.value(), messageFont(), marginSpinner.value(),
        messageColorPicker.value(), messageBorderColorPicker.value(), messageBorderStrengthSpinner.value(),
        messageAlign.selectionModel().getSelectedIndex match {
          case 0 => AlignLeft
          case 1 => AlignCenter
          case 2 => AlignRight
        }, messagePositionSpinner.value(), imageDirectory())
    }
  }

  val pane = new ScrollPane {
    content = if (flying) {
      Flying.pane
    } else {
      Waveform.pane
    }
  }
  val styleComboBox = new ComboBox[String](Seq("Beatmeter Style: Flying Beats", "Beatmeter Style: Waveform")) {
    hgrow = Priority.Always
    maxWidth = Double.PositiveInfinity
    margin = Insets(0, 0, 10, 0)
    style = "-fx-font: 15px sans-serif"
    selectionModel().select(if (flying) {
      0
    } else {
      1
    })
    selectionModel().selectedIndexProperty().onChange { (_, _, i) =>
      i.intValue() match {
        case 0 => pane.content = Flying.pane
        case 1 => pane.content = Waveform.pane
      }
      scene().windowProperty()().sizeToScene()
    }
  }
  dialogPane = new DialogPane {
    headerText = "Beatmeter Settings"
    content = new VBox {
      self =>
      children = Seq(
        styleComboBox,
        pane
      )
    }
    buttonTypes = Seq(ButtonType.Cancel, ButtonType.Apply)
  }
  resultConverter = bt => {
    if (ButtonType.Apply == bt) {
      (styleComboBox.selectionModel().getSelectedIndex == 0, Flying.getConf(), Waveform.getConf())
    } else {
      (flying, flyingConf, waveformConf)
    }
  }
}