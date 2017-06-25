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

package io.gitlab.sklavedaniel.beatmetergenerator

import java.io._
import java.util
import javafx.collections.{FXCollections, ObservableList}
import javafx.scene.input

import org.rogach.scallop.ScallopConf

import scala.collection.JavaConverters.asJavaCollection
import scala.collection.mutable
import scala.util.Try
import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.beans.property.BooleanProperty
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Pos
import scalafx.scene.control.ListView.sfxListView2jfx
import scalafx.scene.control._
import scalafx.scene.control.cell.TextFieldListCell
import scalafx.scene.input._
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Line}
import scalafx.scene.text.Text
import scalafx.scene.{Group, Scene}
import scalafx.stage.FileChooser
import scalafx.util.Duration
import scalafx.util.converter.DoubleStringConverter

object BeatEditor2 {

  class Conf extends Main.ExecutableSubcommand("editor2") {
    val input = opt[File](required = true, descr = "Audio file to play")
    val beats = opt[File](descr = "File containing beat definitions")
    validateFileExists(input)
    validateFileExists(beats)

    def execute(subcommands: List[ScallopConf], args: Array[String]): Unit = {
      new BeatEditor2(this).main(args)
    }
  }

}

class BeatEditor2(conf: BeatEditor2.Conf) extends JFXApp {

  val player = new AudioPlayer2(new BufferedInputStream(new FileInputStream(conf.input())), new BufferedInputStream(getClass.getResourceAsStream("/beats/click.wav")))

  player.ratio.set(0.8)
  player.rate.set(0.5f)

  val beats: ObservableList[Double] = FXCollections.observableList(new util.ArrayList[Double](
    asJavaCollection(conf.beats.toOption.map(BeatFiles.load(_)).getOrElse(Seq[Double]()))
  ))


  val secondWidth = 100
  val volumeHeight = 150
  val positionSlider: Slider = new Slider {
    min = 0
    max = player.duration
    value = 0
    blockIncrement = 1

    player.position.onChange {(_,_,d) =>
      if(!d._2) {
        value() = d._1
      }
    }
  }

  val waveGroup = new Group {
    layoutX = 0
    layoutY = 0
    children = (for (Seq((avg1, time1), (avg2, time2)) <- player.maxs.sliding(2)) yield {
      new Line {
        startX = 100 + time1 * secondWidth
        startY = volumeHeight + 10 - avg1 / Short.MaxValue * volumeHeight
        endX = 100 + time2 * secondWidth
        endY = volumeHeight + 10 - avg2 / Short.MaxValue * volumeHeight
      }
    }).toSeq
  }
  val beatsGroup = new Group {
    layoutX = 0
    layoutY = 0
  }
  val positionLine = new Line {
    startX = 100
    startY = 0
    endX = 100
    endY = 200
    stroke = Color.Red
  }
  val waveView = new Pane {
    pane =>
    layoutX = 0
    layoutY = 0
    style = "-fx-background: blue"
    prefHeight = 160
    children = Seq(
      waveGroup,
      beatsGroup,
      new Line {
        startX = 0
        startY = 160
        endX = player.duration * secondWidth + 10000
        endY = 160
        stroke = Color.DarkGray
        strokeWidth = 0.5
      },
      positionLine
    )
    onMouseClicked = e => {
      if (MouseButton.Primary.equals(e.getButton) && e.getClickCount == 2) {
        positionSlider.value() = ((e.getX - 100) / secondWidth).max(0).min(player.duration)
      }
    }
  }

  class Beat(val time: Double, initSelected: Boolean) extends Circle {
    radius = 4
    centerY = 170
    centerX = 100 + time * secondWidth
    val selected = BooleanProperty(initSelected)
    fill <== when(selected) choose Color.Blue otherwise Color.Red
    var clickDelay: Option[Timeline] = None
    onMouseClicked = e => {
      clickDelay.foreach(_.stop())
      clickDelay = None
      if (MouseButton.Primary.equals(e.getButton)) {
        if (e.getClickCount == 1) {
          val t = Timeline(KeyFrame(Duration(300), onFinished = _ => {

          }))
          t.play()
          clickDelay = Some(t)
        } else if (e.getClickCount == 2) {
          positionSlider.value() = time
        }
      }
      e.consume()
    }
  }

  val beatIcons = mutable.Map[Double, Beat]()

  for (beat <- beats) {
    val b = new Beat(beat, false)
    beatIcons.put(beat, b)
    beatsGroup.children.add(b)
  }

  stage = new PrimaryStage {
    self =>
    scene = new Scene {
      root = new VBox {
        prefWidth = 800
        spacing = 5
        children = Seq(
          new Pane {
            minHeight = 200
            children = Seq(waveView)
          },
          positionSlider,
          new ToggleButton("play") {
            selected <==> player.playing
          }
        )
      }
    }
  }

}

