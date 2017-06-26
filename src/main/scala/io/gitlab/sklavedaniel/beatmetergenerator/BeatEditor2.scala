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
import javafx.scene.{Cursor, input}

import org.rogach.scallop.ScallopConf

import scala.collection.JavaConverters.asJavaCollection
import scala.collection.mutable
import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.{JFXApp, Platform}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.binding.Bindings
import scalafx.beans.property._
import scalafx.scene.control._
import scalafx.scene.input._
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Line, Rectangle}
import scalafx.scene.transform.Scale
import scalafx.scene.{Group, Scene}
import scalafx.util.Duration

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

    player.position.onChange { (_, _, d) =>
      if (!d._2) {
        value() = d._1
      }
    }
  }

  class WaveView(initPoints: Seq[((Double, Double), Double)], initPxPerSec: Double, initMaxVolume: Double, initHeight: Double) extends Group {
    val points = ObjectProperty(initPoints)
    val pxPerSec = DoubleProperty(initPxPerSec)
    val maxVolume = DoubleProperty(initMaxVolume)
    val height = DoubleProperty(initHeight)

    private def update() = {
      children = (for (Seq((value1, time1), (value2, time2)) <- points().sliding(2)) yield {
        new Line {
          startX <== pxPerSec * time1
          startY <== height * (maxVolume + value1._2) / maxVolume / 2
          endX <== pxPerSec * time2
          endY <== height * (maxVolume + value2._2) / maxVolume / 2
        }
      }).toIterable ++ (for (Seq((value1, time1), (value2, time2)) <- points().sliding(2)) yield {
        new Line {
          startX <== pxPerSec * time1
          startY <== height * (maxVolume - value1._1) / maxVolume / 2
          endX <== pxPerSec * time2
          endY <== height * (maxVolume - value2._1) / maxVolume / 2
        }
      })
    }

    update()

    points.onChange(update())

    val scale = {
      val s = new Scale(1.0, 1.0, 0.0, 0.0)
      transforms = Seq(s)
      s.x
    }
  }

  class PositionLineView(initPxPerSec: Double, initHeight: Double, initWidth: Double, sceeneToContextX: Double => Double) extends Group {
    self =>
    val position = ObjectProperty((0.0, true))
    val pxPerSec = DoubleProperty(initPxPerSec)
    val scale = DoubleProperty(1.0)
    val height = DoubleProperty(initHeight)
    val width = DoubleProperty(initWidth)
    val duration = DoubleProperty(0.0)
    private val pxPosition_ = ReadOnlyDoubleWrapper(0.0)
    pxPosition_ <== pxPerSec * scale * Bindings.createDoubleBinding(() => position()._1, position)
    val pxPosition = pxPosition_.readOnlyProperty

    children = Seq(
      new Rectangle {
        x <== pxPosition - self.width / 2
        y = 0
        width <== self.width
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
    val dragging =dragging_.getReadOnlyProperty

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
    }
    onMouseReleased = e => {
      self.setCursor(Cursor.HAND)
      dragging_() = false
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

  val mainView = new ScrollPane {
    val waveView = new WaveView(player.maxima, 20, player.maxima.map(v => v._1._1 max v._1._2).max, 200)
    val sceeneToContextX = (x: Double) => sceneToLocal(x, 0.0).getX + scrollX
    val positionLineView = new PositionLineView(20, 300, 8, sceeneToContextX) {
      position <==> player.position
      scale <== waveView.scale
      duration <== player.duration
    }
    content = new Group {
      children = Seq(
        waveView,
        positionLineView
      )
    }

    def scrollX = hvalue() * (content().boundsInLocal().getWidth - viewportBounds().getWidth)

    def scrollX_=(x: Double): Unit = {
      hvalue() = x / (content().boundsInLocal().getWidth - viewportBounds().getWidth)
    }

    def scrollY = vvalue() * (content().boundsInLocal().getHeight - viewportBounds().getHeight)

    def scrollY_=(x: Double): Unit = {
      vvalue() = x / (content().boundsInLocal().getHeight - viewportBounds().getHeight)
    }

    private var delta = 0.0
    private var timeout: Option[Timeline] = None
    addEventFilter(ScrollEvent.Scroll, (e: input.ScrollEvent) => {
      e.consume()
      if (e.isControlDown) {
        timeout.foreach(_.stop())
        delta += e.getDeltaY
        val to = Timeline(KeyFrame(time = Duration(25), onFinished = _ => {
          val oldPosition = (scrollX + e.getX) / waveView.scale()
          waveView.scale() = (waveView.scale() * (1 + delta / 400)).max(0.5).min(10.0)
          scrollX = oldPosition * waveView.scale() - e.getX()
          delta = 0.0
        }))
        to.play()
        timeout = Some(to)
      } else if (e.isShiftDown) {
        scrollY += e.getDeltaY()
      } else {
        scrollX += e.getDeltaY()
      }
    })

    val minX = DoubleProperty(0.05)
    val maxX = DoubleProperty(0.95)

    positionLineView.pxPosition.onChange { (_, _, v) =>
      if (v.doubleValue() < scrollX + minX() * viewportBounds().getWidth) {
        scrollX = v.doubleValue() - minX() * viewportBounds().getWidth
      } else if (v.doubleValue() > scrollX + maxX() * viewportBounds().getWidth) {
        scrollX = v.doubleValue() - maxX() * viewportBounds().getWidth
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


  stage = new PrimaryStage {
    self =>
    scene = new Scene {
      root = new BorderPane {
        prefWidth = 800
        center = mainView
        bottom = new VBox {
          children = Seq(
            new ToggleButton("play") {
              selected <==> player.playing
            }
          )
        }
      }
    }
  }

}

