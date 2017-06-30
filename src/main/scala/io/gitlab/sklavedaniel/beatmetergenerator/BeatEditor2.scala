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
import javafx.beans.{InvalidationListener, WeakInvalidationListener}
import javafx.beans.value.{ChangeListener, WeakChangeListener}
import javafx.geometry.VPos
import javafx.scene.{Cursor, input}

import io.gitlab.sklavedaniel.beatmetergenerator.utils.ObservableIntervalMap
import org.rogach.scallop.ScallopConf

import scala.collection.mutable
import scala.math.Ordering._
import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.binding.Bindings
import scalafx.beans.property._
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
import scalafx.scene.input._
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Line, Rectangle}
import scalafx.scene.text.{Font, Text}
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

  println("starting...")
  val player = new AudioPlayer2(new BufferedInputStream(new FileInputStream(conf.input())), new BufferedInputStream(getClass.getResourceAsStream("/beats/click.wav")))

  player.ratio.set(0.8)
  player.rate.set(0.5f)

  class WaveView(initPoints: Seq[((Double, Double), Double)], initPxPerSec: Double, initMaxVolume: Double, initHeight: Double, sceneToContextX: Double => Double) extends Group {
    self =>
    val points = ObjectProperty(initPoints)
    val pxPerSec = DoubleProperty(initPxPerSec)
    val maxVolume = DoubleProperty(initMaxVolume)
    val height = DoubleProperty(initHeight)

    private def update() = {
      children += new Rectangle {
        x = 0.0
        y = 0.0
        width <== pxPerSec * points().last._2
        height <== self.height
        fill = Color.Transparent
      }.delegate
      for (Seq((value1, time1), (value2, time2)) <- points().sliding(2)) {
        children += new Line {
          startX <== pxPerSec * time1
          startY <== height * (maxVolume + value1._1) / maxVolume / 2
          endX <== pxPerSec * time2
          endY <== height * (maxVolume + value2._1) / maxVolume / 2
          strokeWidth = 0.5
        }.delegate
      }
      for (Seq((value1, time1), (value2, time2)) <- points().sliding(2)) {
        children += new Line {
          startX <== pxPerSec * time1
          startY <== height * (maxVolume - value1._2) / maxVolume / 2
          endX <== pxPerSec * time2
          endY <== height * (maxVolume - value2._2) / maxVolume / 2
          strokeWidth = 0.5
        }.delegate
      }
    }

    update()

    points.onChange(update())

    val scale = {
      val s = new Scale(1.0, 1.0, 0.0, 0.0)
      transforms = Seq(s)
      s.x
    }

    val onAction = ObjectProperty((p: Double) => ())

    onMouseClicked = mouseHandler(_ => (), e => {
      onAction()(sceneToContextX(e.getSceneX) / pxPerSec() / scale())
    })
  }

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

  sealed trait TrackElement

  class Beat() extends TrackElement {
    val name = ObjectProperty("")
    val highlight = BooleanProperty(false)
  }

  class BPMGroup() extends TrackElement {
    val bpm = DoubleProperty(0.0)
    val beat = new Beat()
  }

  class BeatsPattern() extends TrackElement {
    val patternDuration = DoubleProperty(0.0)
    val pattern = ObservableIntervalMap[Double, Beat](math.Ordering.Double)
  }

  class Message() extends TrackElement {
    val text = ObjectProperty("")
  }


  class Track {
    val title = ObjectProperty("")
    val content = ObservableIntervalMap[Double, TrackElement](math.Ordering.Double)
  }

  class TrackHeaderView(val track: Track) extends Group {
    self =>
    val width = DoubleProperty(0.0)
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
          new Button {
            text = "X"
            font = Font(8)
            focusTraversable = false
          },
          new ToggleButton {
            text = "P"
            font = Font(8)
            focusTraversable = false
          },
          new ToggleButton {
            text = "S"
            font = Font(8)
            focusTraversable = false
          }
        )
      }
    )
  }

  class TrackView(initPxPerSec: Double, val track: Track) extends Group {
    val duration = DoubleProperty(0.0)
    val pxPerSec = DoubleProperty(initPxPerSec)
    val scale = {
      val s = new Scale(1.0, 1.0, 0.0, 0.0)
      transforms = Seq(s)
      s.x
    }
    val labelX = DoubleProperty(0.0)

    val elementGroup = new Group {
      layoutY = 25
      layoutX = 0
    }
    private val element2view = mutable.Map[(Double, Double, TrackElement), TrackElementView]()

    private def addElement(elem: (Double, Double, TrackElement)): Unit = {
      val v = elem._3 match {
        case beat: Beat =>
          new BeatView(beat)
        case beatsGroup: BeatsPattern =>
          new BeatsPatternView(beatsGroup)
        case bpmGroup: BPMGroup =>
          new BPMGroupView(bpmGroup)
        case message: Message =>
          new MessageView(message)
      }
      v.layoutX <== pxPerSec * elem._1
      v.pxPerSec <== pxPerSec
      v.position() = (elem._1, elem._2)
      v.position.onChange { (_, old, pos) =>
        if (old != pos) track.content.move(old, pos)
      }
      element2view(elem) = v
      elementGroup.children.add(v)
    }

    private val listener = ObservableIntervalMap.WeakChangeListner[Double, TrackElement] {
      case (_, ObservableIntervalMap.AddChange(l: List[(Double, Double, TrackElement)])) =>
        for (elem <- l) {
          addElement(elem)
        }
      case (_, ObservableIntervalMap.MoveChange(from, to, b)) =>
        if (from != to) element2view((from._1, from._2, b)).position() = to

      case (_, ObservableIntervalMap.RemoveChange(l: List[(Double, Double, TrackElement)])) =>
        for (elem <- l) {
          element2view.remove(elem).foreach(elementGroup.children.remove)
        }
    }
    track.content.addListener(listener)
    for (elem <- track.content) {
      addElement(elem)
    }


    children = Seq(
      new Rectangle {
        x = 0.0
        y = 0.0
        height = 50.0
        width <== duration * pxPerSec
        fill = Color.Transparent
      },
      new Line {
        startY = 25
        startX = 0
        endY = 25
        endX <== duration * pxPerSec
      },
      elementGroup
    )
  }

  sealed class TrackElementView extends Group {
    val pxPerSec = DoubleProperty(0.0)
    val position = ObjectProperty((0.0, 0.0))
    val duration = Bindings.createDoubleBinding(() => position()._2 - position()._1, position)
    val width = pxPerSec * duration
  }

  final class BeatView(val beat: Beat) extends TrackElementView {
    self =>
    children = Seq(new Rectangle {
      width <== self.width
      height = 10
      y = -5
      fill = Color.DarkRed
    })
  }

  final class BeatsPatternView(val beatsPattern: BeatsPattern) extends TrackElementView {
    self =>
    val beatsGroup = new Group
    val repetitionsGroup = new Group
    children = Seq(
      new Rectangle {
        width <== when(self.width < 5) choose 5 otherwise self.width
        height = 20
        y = -10
        fill = Color.LightPink
      },
      beatsGroup,
      repetitionsGroup,
      new Line {
        startX <== beatsPattern.patternDuration * pxPerSec
        endX <== beatsPattern.patternDuration * pxPerSec
        startY = -10.0
        endY = 10.0
        strokeWidth = 0.5
      }
    )

    private def update(): Unit = {
      beatsGroup.children = (for ((start, end, b) <- beatsPattern.pattern) yield {
        val v = new BeatView(b)
        v.position() = (start, end)
        v.pxPerSec <== pxPerSec
        v.layoutX <== pxPerSec * start
        v
      }).toSeq
      repetitionsGroup.children = for {
        i <- 0 until (duration.get() / beatsPattern.patternDuration()).floor.toInt
        (start, end, b) <- beatsPattern.pattern
      } yield {
        val v = new BeatView(b)
        v.position() = (i * beatsPattern.patternDuration() + start, end)
        v.pxPerSec <== pxPerSec
        v.layoutX <== pxPerSec * (i * beatsPattern.patternDuration() + start)
        v
      }
    }

    update()

    val handler = weak(update())
    duration.addListener(handler)
    beatsPattern.pattern.addListener(handler)
    beatsPattern.patternDuration.addListener(handler)
  }

  final class BPMGroupView(val bpmGroup: BPMGroup) extends TrackElementView {
    private val binding = when(width < 5) choose 5 otherwise width
    val beatsGroup = new Group
    children = Seq(
      new Rectangle {
        width <== binding
        height = 20
        y = -10
        fill = Color.LightBlue
      },
      beatsGroup
    )

    private def update(): Unit = {
      beatsGroup.children = if (bpmGroup.bpm() > 0) {
        for (i <- 0 to (duration.get() * 60 / bpmGroup.bpm()).floor.toInt) yield {
          val v = new BeatView(bpmGroup.beat)
          v.pxPerSec <== pxPerSec
          val p = i * bpmGroup.bpm() / 60
          v.position() = (p, p + 0.05)
          v.layoutX <== pxPerSec * p
          v
        }
      } else {
        Seq()
      }
    }

    update()
    private val handler = weak(update())
    duration.addListener(handler)
    bpmGroup.bpm.addListener(handler)

  }

  final class MessageView(val message: Message) extends TrackElementView {
    self =>
    children = Seq(
      new Rectangle {
        width <== when(self.width < 5) choose 5 otherwise self.width
        height = 20
        y = -10
        fill = Color.LightGreen
      },
      new Text {
        x = 3.0
        y = 0.0
        clip = new Rectangle {
          y = -8
          width <== when(self.width < 5) choose 5 otherwise self.width
          height = 16
        }
        text <== message.text
        textOrigin = VPos.CENTER
      }
    )
  }

  val beats = conf.beats.toOption.map(BeatFiles.load(_).map(t => (t, t + 0.05, new Beat()))).getOrElse(Seq[(Double, Double, Beat)]())

  val mainView = new HBox {

    val tracks = new ObservableBuffer[Track]()

    val headerBox = new VBox {
      spacing = 5
    }

    object scrollPane extends ScrollPane {
      self =>
      val sceeneToContextX = (x: Double) => sceneToLocal(x, 0.0).getX + scrollX
      println("generating waveform...")
      val waveView = new WaveView(player.maxima, 20, player.maxima.toIterator.map(v => v._1._1 max v._1._2).max, 200, sceeneToContextX)
      println("waveform generated.")

      val tracksBox = new VBox {
        spacing = 5
      }

      private val track2view = mutable.Map[Track, TrackView]()
      private val track2headerView = mutable.Map[Track, TrackHeaderView]()

      tracks.onChange { (_, cs) =>
        for (c <- cs) {
          c match {
            case ObservableBuffer.Add(i, ts) =>
              for (t <- ts) {
                val v = new TrackView(20, t) {
                  scale <== waveView.scale
                  duration <== player.duration
                }
                val h = new TrackHeaderView(t) {
                  width <== headerBox.width
                }
                track2view(t) = v
                track2headerView(t) = h
                tracksBox.children.add(i, v)
                headerBox.children.add(i, h)
              }
            case ObservableBuffer.Remove(_, ts) =>
              for (t <- ts) {
                track2view.remove(t).foreach(tracksBox.children.remove)
                track2headerView.remove(t).foreach(headerBox.children.remove)
              }
            case _ => assert(false)
          }
        }
      }

      val box = new VBox {
        spacing = 5
        children = Seq(
          waveView,
          tracksBox
        )
      }

      val positionLineView = new PositionLineView(20, 10, sceeneToContextX) {
        position <==> player.position
        scale <== waveView.scale
        duration <== player.duration
        height <== box.height
      }

      content = new Group {
        children = Seq(
          box,
          positionLineView
        )
      }

      def scrollX = hvalue() * (content().boundsInLocal().getWidth - viewportBounds().getWidth).max(0.0)

      def scrollX_=(x: Double): Unit = {
        hvalue() = (x / (content().boundsInLocal().getWidth - viewportBounds().getWidth)).max(hmin()).min(hmax())
      }

      def scrollY = vvalue() * (content().boundsInLocal().getHeight - viewportBounds().getHeight).max(0.0)

      def scrollY_=(x: Double): Unit = if ((content().boundsInLocal().getHeight - viewportBounds().getHeight) > 0) {
        vvalue() = (x / (content().boundsInLocal().getHeight - viewportBounds().getHeight)).max(vmin()).min(vmax())
      }

      addEventFilter(ScrollEvent.Scroll, (e: input.ScrollEvent) => {
        e.consume()
        if (e.isControlDown) {
          val oldPosition = (scrollX + e.getX) / waveView.scale()
          waveView.scale() = (waveView.scale() * (1 + e.getDeltaY / 400)).max(1.0).min(10.0)
          scrollX = oldPosition * waveView.scale() - e.getX()
        } else if (e.isShiftDown) {
          scrollY -= e.getDeltaY()
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

      waveView.onAction() = p => {
        positionLineView.position() = (p, true)
      }

    }

    val headerGroup = new Pane {
      self =>
      val box = new Group {
        layoutY <== Bindings.createDoubleBinding(() => -scrollPane.scrollY, scrollPane.vvalue, scrollPane.viewportBounds, scrollPane.content().boundsInLocal)
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
    children = Seq(
      headerGroup,
      scrollPane
    )

    tracks.append(new Track() {
      title() = "very long title"
      content ++= beats
    })
    tracks.append(new Track() {
      title() = "foo"
      content ++= Seq(
        (0.5, 10.0, new BPMGroup() {
          bpm() = 120
        }),
        (20, 100.0, new BeatsPattern() {
          pattern ++= Seq(
            (0.5, 0.55, new Beat()),
            (1.5, 1.55, new Beat()),
            (3.5, 3.55, new Beat()),
            (4.5, 4.55, new Beat())
          )
          patternDuration() = 10.0
        }),
        (150, 200.0, new Message() {
          text() = "hello world"
        })
      )
    })

  }

  def mouseHandler(single: MouseEvent => Unit, double: MouseEvent => Unit) = {
    var clickDelay: Option[Timeline] = None
    (e: MouseEvent) => {
      clickDelay.foreach(_.stop())
      clickDelay = None
      if (e.getClickCount == 1) {
        val t = Timeline(KeyFrame(Duration(300), onFinished = _ => {
          single(e)
        }))
        t.play()
        clickDelay = Some(t)
      } else if (e.getClickCount == 2) {
        double(e)
      }
    }
  }


  stage = new PrimaryStage {
    self =>
    scene = new Scene {
      root = new BorderPane {
        prefWidth = 800
        top = new MenuBar {
          menus = Seq(
            new Menu("File") {
              items = Seq(
                new MenuItem("Open"),
                new MenuItem("Import Beats"),
                new MenuItem("Export Beats")
              )
            }
          )
        }
        center = mainView
        bottom = new ToolBar {
          items = Seq(
            new ToggleButton("play") {
              selected <==> player.playing
              focusTraversable = false
            },
            new ToggleButton("beat") {
              tooltip = Tooltip("Insert beat at current position")
              focusTraversable = false
            }
          )
        }
      }
    }
  }

  def weak[A](listener: ChangeListener[A]): ChangeListener[A] = new WeakChangeListener[A](listener)

  def weak(listener: => Unit): InvalidationListener = new WeakInvalidationListener(_ => listener)

  println("started.")
}

