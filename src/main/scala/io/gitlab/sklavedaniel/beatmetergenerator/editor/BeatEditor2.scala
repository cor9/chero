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

import java.io._
import javafx.beans.binding.{DoubleBinding, DoubleExpression}
import javafx.beans.value.{ChangeListener, WeakChangeListener}
import javafx.beans.{InvalidationListener, WeakInvalidationListener}
import javafx.geometry.VPos
import javafx.scene.{Cursor, input}

import io.gitlab.sklavedaniel.beatmetergenerator.utils.{BeatFiles, ObservableIntervalMap}
import io.gitlab.sklavedaniel.beatmetergenerator._
import org.rogach.scallop.ScallopConf

import scala.collection.mutable
import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.binding.{Bindings, ObjectExpression}
import scalafx.beans.property._
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.control.{MenuItem, _}
import scalafx.scene.input._
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.scene.text.{Font, Text}
import scalafx.scene.transform.Scale
import scalafx.scene.{Group, Scene}
import scalafx.stage.FileChooser
import scalafx.stage.FileChooser.ExtensionFilter
import scalafx.util.Duration

object BeatEditor2 {

  class Conf extends Main.ExecutableSubcommand("editor2") {
    val input = opt[File](descr = "Audio file to play")
    val beats = opt[File](descr = "File containing beat definitions")
    validateFileExists(input)
    validateFileExists(beats)

    def execute(subcommands: List[ScallopConf], args: Array[String]): Unit = {
      new BeatEditor2(this).main(args)
    }
  }

}

class BeatEditor2(conf: BeatEditor2.Conf) extends JFXApp {

  val audioFile = conf.input.orElse(Option({
    val fc = new FileChooser()
    fc.title = "Beatmeter Generator: Open audio file"
    fc.getExtensionFilters += new ExtensionFilter("wav audio file (16bit unsigned)", "*.wav")
    fc.showOpenDialog(null)
  }))
  if (audioFile.isEmpty) {
    System.exit(1)
  }

  println("starting...")
  val player = new AudioPlayer2(new BufferedInputStream(new FileInputStream(audioFile())), new BufferedInputStream(getClass.getResourceAsStream("/beats/click.wav")))

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


  class TrackHeaderView(val track: Track) extends Group {
    self =>
    val width = DoubleProperty(0.0)
    val active = new ToggleButton {
      text = "P"
      tooltip = new Tooltip("mute/unmute track") {
        font = Font(10)
      }
      font = Font(8)
      focusTraversable = false
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
          new Button {
            text = "X"
            tooltip = new Tooltip("delete track") {
              font = Font(10)
            }
            font = Font(8)
            focusTraversable = false
          },
          active,
          new ToggleButton {
            text = "R"
            tooltip = new Tooltip("record beats to track") {
              font = Font(10)
            }
            font = Font(8)
            focusTraversable = false
          },
          new ToggleButton {
            text = "S"
            tooltip = new Tooltip("use track for snapping") {
              font = Font(10)
            }
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
    val beats = ObservableIntervalMap[Double, Beat]
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

    private def addGenerator(gen: BeatsGenerator, tmp: TrackElementView): Unit = {
      beats ++= gen.beats().map(e => (e._1 + tmp.position()._1, e._2 + tmp.position()._1, e._3)).takeWhile(_._2 <= tmp.position()._2)

      gen.beats.onInvalidate {
        beats --= beats.intersecting(tmp.position()._1, tmp.position()._2).map(e => (e._1, e._2))
        beats ++= gen.beats().map(e => (e._1 + tmp.position()._1, e._2 + tmp.position()._1, e._3)).takeWhile(_._2 <= tmp.position()._2)
      }
      tmp.position.onChange { (_, old, pos) =>
        beats --= beats.intersecting(old._1, old._2).map(e => (e._1, e._2))
        beats ++= gen.beats().map(e => (e._1 + tmp.position()._1, e._2 + tmp.position()._1, e._3)).takeWhile(_._2 <= tmp.position()._2)
      }
    }

    private def addElement(elem: (Double, Double, TrackElement)): Unit = {
      val v = elem._3 match {
        case beat: Beat =>
          beats ++= Seq((elem._1, elem._2, beat))
          new BeatView(beat, true, scale)
        case beatsGroup: BeatsPattern =>
          val tmp = new BeatsPatternView(beatsGroup, scale, track.content)
          addGenerator(beatsGroup, tmp)
          tmp
        case bpmGroup: BPMPattern =>
          val tmp = new BPMGroupView(bpmGroup, scale, track.content)
          addGenerator(bpmGroup, tmp)
          tmp
        case message: Message =>
          new MessageView(message, scale, track.content)
      }
      v.layoutX <== pxPerSec * elem._1
      v.pxPerSec <== pxPerSec
      v.position() = (elem._1, elem._2)
      v.position.onChange { (_, old, pos) =>
        if (old != pos && !track.content.contains(pos._1, pos._2)) track.content.move(old, pos, !_.isInstanceOf[Beat])
      }
      element2view(elem) = v
      elementGroup.children.add(v)
    }

    private val listener = ObservableIntervalMap.WeakChangeListner[Double, TrackElement] {
      case (_, ObservableIntervalMap.AddChange(l: List[(Double, Double, TrackElement)])) =>
        for (elem <- l) {
          addElement(elem)
        }
      case (_, ObservableIntervalMap.MoveChange(from, to, l)) =>
        if (from != to) {
          for ((f, t, b) <- l) {
            val v = element2view.remove((f._1, f._2, b)).get
            element2view += (t._1, t._2, b) -> v
            v.position() = t
          }
        }
      case (_, ObservableIntervalMap.RemoveChange(l: List[(Double, Double, TrackElement)])) =>
        for (elem <- l) {
          element2view.remove(elem).foreach(elementGroup.children.remove)
          beats --= beats.intersecting(elem._1, elem._2).map(e => (e._1, e._2))
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

  sealed class TrackElementView(val scaled: DoubleExpression) extends Group {
    val pxPerSec = DoubleProperty(0.0)
    val position = ObjectProperty((0.0, 0.0))
    val duration = Bindings.createDoubleBinding(() => position()._2 - position()._1, position)
    val width = pxPerSec * duration
  }

  sealed class ResizableElementView(scaled: DoubleExpression, context: ObservableIntervalMap[Double, TrackElement]) extends TrackElementView(scaled) {
    self =>
    onMouseMoved = e => {
      if (e.getX > width.doubleValue() - 5 / scaled.doubleValue()) {
        self.setCursor(Cursor.H_RESIZE)
      } else {
        self.setCursor(Cursor.DEFAULT)
      }
    }
    onMouseDragged = e => {
      if (resizeOffset.isDefined) {
        val tmp = position()._1
        val newDuration = minDuration.max((e.getX + resizeOffset.get).max(0.05) / pxPerSec())
        val tmp2 = tmp + newDuration
        if (context.intersecting(tmp, tmp2).forall(p => tmp <= p._1 && p._2 <= position()._2)) {
          position() = (tmp, tmp2)
        }
      }
    }

    def minDuration = 0.0

    private var resizeOffset: Option[Double] = None
    onMousePressed = e => {
      if (e.isPrimaryButtonDown && e.getX > width.doubleValue() - 5 / scaled.doubleValue()) {
        resizeOffset = Some(width.doubleValue() - e.getX)
      }
    }
    onMouseReleased = e => {
      resizeOffset = None
      if (e.getX < width.doubleValue() - 5 / scaled.doubleValue() || !contains(e.getX, e.getY))
        self.setCursor(Cursor.DEFAULT)
    }
  }

  final class BeatView(val beat: Beat, val editable: Boolean, scaled: DoubleExpression) extends TrackElementView(scaled) {
    self =>
    children = Seq(new Rectangle {
      width <== self.width
      height = 10
      y = -5
      fill = if (editable) Color.DarkRed else Color.DarkGray
    })
  }

  final class BeatsPatternView(val beatsPattern: BeatsPattern, scaled: DoubleExpression, context: ObservableIntervalMap[Double, TrackElement]) extends ResizableElementView(scaled, context) {
    self =>
    val beatsGroup = new Group
    children = Seq(
      new Rectangle {
        width <== when(self.width < 5) choose 5 otherwise self.width
        height = 20
        y = -10
        fill = Color.LightPink
      },
      beatsGroup,
      new Line {
        self =>
        startX <== beatsPattern.patternDuration * pxPerSec
        endX <== beatsPattern.patternDuration * pxPerSec
        startY = -10.0
        endY = 10.0
        strokeWidth = 0.5
        onMouseMoved = e => {
          self.delegate.setCursor(Cursor.H_RESIZE)
        }
        onMouseDragged = e => {
          val old = beatsPattern.patternDuration()
          beatsPattern.patternDuration() = e.getX.max(0.05) / pxPerSec()
          if (e.isControlDown) {
            beatsPattern.pattern.move((0, old), (0, beatsPattern.patternDuration()), !_.isInstanceOf[Beat])
          }
        }
        onMouseReleased = e => {
          self.delegate.setCursor(Cursor.DEFAULT)
        }
      }
    )

    private def update(): Unit = {
      beatsGroup.children = for ((start, end, b) <- beatsPattern.beats().takeWhile(_._2 < duration.get())) yield {
        val v = new BeatView(b, start < beatsPattern.patternDuration(), scaled)
        v.position() = (start, end)
        v.pxPerSec <== pxPerSec
        v.layoutX <== pxPerSec * start
        v
      }
    }

    update()
    private val handler: InvalidationListener = _ => update()
    duration.addListener(weak(handler))
    beatsPattern.beats.addListener(weak(handler))

    override def minDuration = beatsPattern.patternDuration()
  }

  final class BPMGroupView(val bpmGroup: BPMPattern, scaled: DoubleExpression, context: ObservableIntervalMap[Double, TrackElement]) extends ResizableElementView(scaled, context) {
    self =>
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
      beatsGroup.children = for ((start, end, b) <- bpmGroup.beats().takeWhile(_._2 < duration.get())) yield {
        val v = new BeatView(b, false, scaled)
        v.pxPerSec <== pxPerSec
        v.position() = (start, end)
        v.layoutX <== pxPerSec * start
        v
      }
    }

    update()
    private val handler: InvalidationListener = _ => update()
    duration.addListener(weak(handler))
    bpmGroup.beats.addListener(weak(handler))

  }

  final class MessageView(val message: Message, scaled: DoubleExpression, context: ObservableIntervalMap[Double, TrackElement]) extends ResizableElementView(scaled, context) {
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
      override def requestFocus() {

      }

      focusTraversable = false

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
              for (t <- ts) yield {
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
                player.beats.add(i, (h.active.selected, v.beats))
              }
            case ObservableBuffer.Remove(i, ts) =>
              for (t <- ts) {
                track2view.remove(t).foreach(tracksBox.children.remove)
                track2headerView.remove(t).foreach(headerBox.children.remove)
                player.beats.remove(i)
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
        (0.5, 10.0, new BPMPattern() {
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
    title = "Beatmeter Generator"
    scene = new Scene {
      root = new BorderPane {
        prefWidth = 800
        top = new MenuBar {
          menus = Seq(
            new Menu("File") {
              items = Seq(
                new MenuItem("Open"),
                new MenuItem("Save"),
                new MenuItem("Import Beats"),
                new MenuItem("Export Beats")
              )
            },
            new Menu("Edit") {
              items = Seq(
                new MenuItem("Undo"),
                new MenuItem("Redo")
              )
            },
            new Menu("Tools") {
              items = Seq(
                new MenuItem("Generate Audio"),
                new MenuItem("Generate Beatmeter")
              )
            }
          )
        }
        center = mainView
        bottom = new ToolBar {
          padding = Insets(0, 0, 0, 0)
          items = Seq(
            new ToggleButton("play") {
              selected <==> player.playing
              focusTraversable = false
            },
            new ToggleButton("beat") {
              tooltip = Tooltip("Insert beat at current position")
              focusTraversable = false
            },
            new HBox {
              hgrow = Priority.Always
            },
            new GridPane {
              scaleX = 0.8
              scaleY = 0.8
              hgap = 5
              vgap = 5
              addRow(0,
                new Text("Speed"),
                new Slider(0.2f, 1.0f, 1.0f) {
                  blockIncrement = 0.1f
                  value <==> player.rate
                }.delegate
              )
              addRow(1,
                new Text("Volume"),
                new Slider(0.0, 1.0, 0.5) {
                  blockIncrement = 0.1
                  value <==> player.ratio
                }.delegate
              )
            }
          )
        }
      }
    }
  }

  def weak[A](listener: ChangeListener[A]): ChangeListener[A] = new WeakChangeListener[A](listener)

  def weak(listener: InvalidationListener): InvalidationListener = new WeakInvalidationListener(listener)

  println("started.")
}

