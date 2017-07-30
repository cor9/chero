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
import scala.reflect.runtime.universe._
import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.binding.{Bindings, ObjectExpression}
import scalafx.beans.property._
import scalafx.collections.ObservableSet.{Add, Remove}
import scalafx.collections.{ObservableBuffer, ObservableSet}
import scalafx.geometry.Insets
import scalafx.scene.control.{MenuItem, _}
import scalafx.scene.input._
import scalafx.scene.layout._
import scalafx.scene.paint.{Color, Paint}
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.scene.text.{Font, Text}
import scalafx.scene.transform.Scale
import scalafx.scene.{Group, Node, Scene}
import scalafx.stage.FileChooser
import scalafx.stage.FileChooser.ExtensionFilter
import scalafx.util.Duration

object BeatEditor2 {

  val dataformat = new DataFormat("beats")

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
      e.consume()
    }
    onMouseReleased = e => {
      self.setCursor(Cursor.HAND)
      dragging_() = false
      e.consume()
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
    val snap = new ToggleButton {
      text = "S"
      tooltip = new Tooltip("use track for snapping") {
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
          snap,
          new ToggleButton {
            text = "V"
            tooltip = new Tooltip("use track for beatmeter") {
              font = Font(10)
            }
            font = Font(8)
            focusTraversable = false
          }
        )
      }
    )
  }

  class TrackView(initPxPerSec: Double, val track: Track) extends Group with SelectionContainer[TrackElement] {
    val content = track.content
    val clazz = classOf[ImmutableTrackElement]
    val duration = DoubleProperty(0.0)
    override val pxPerSec = DoubleProperty(initPxPerSec)
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
    val element2view = mutable.Map[(Double, Double, TrackElement), TrackElementView[TrackElement]]()

    override def getView(key: (Double, Double, TrackElement)): TrackElementView[TrackElement] = element2view(key)

    override def startPosition = 0.0

    private def addGenerator(gen: BeatsGenerator, tmp: TrackElementView[_]): Unit = {
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
          element2view.remove(elem).foreach { v =>
            elementGroup.children.remove(v)
            selectedElements.remove(v)
          }
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

  trait SelectionContainer[A <: TrackElement] {
    self: Group =>
    def clazz: Class[_]

    def startPosition: Double

    def pxPerSec: DoubleProperty

    def content: ObservableIntervalMap[Double, A]

    def getView(key: (Double, Double, A)): TrackElementView[A]

    def origin = 0.0

    val selectedElements = ObservableSet.empty[TrackElementView[A]]
    selectedElements.onChange { (_, c) =>
      c match {
        case Add(a) =>
          a.selected() = true
        case Remove(r) =>
          r.selected() = false
      }
    }

    private var selectionBox: Option[Rectangle] = None
    private var selectionPos: Option[Double] = None
    var pressedX: Option[Double] = None
    onMousePressed = e => {
      if (selectionActive(e)) {
        pressedX = Some(e.getX)
        val tmp = content.intersecting(e.getX / pxPerSec(), e.getX / pxPerSec())
        if (tmp.isEmpty) {
          selectionPos = Some(e.getX)
        }
        e.consume()
      }
    }
    var dragRemoved: List[(Double, Double, A)] = Nil

    def selectionActive(e: MouseEvent) = true

    def selectionActive(e: DragEvent) = true

    onDragDetected = e => {
      if (selectionActive(e)) {
        pressedX.foreach { v =>
          val x = v / pxPerSec()
          val tmp = content.intersecting(x, x)
          if (tmp.nonEmpty) {
            val view = getView(tmp.head)
            if (!view.selected()) {
              selectedElements.clear()
              selectedElements += view
            }
            val snapOffset = x - tmp.head._1
            val data = (x, snapOffset, selectedElements.toList.map(x => (x.position(), x.element.toImmutable())))
            if (!e.isControlDown) {
              dragRemoved = selectedElements.toList.map(x => (x.position()._1, x.position()._2, x.element))
              content --= dragRemoved.map(x => (x._1, x._2))
            }
            val cb = startDragAndDrop(if (e.isControlDown) TransferMode.Copy else TransferMode.Move)
            val cc = new input.ClipboardContent()
            cc.put(BeatEditor2.dataformat, data)
            cb.delegate.setContent(cc)
          }
        }
        e.consume()
      }
    }
    onDragDone = e => {
      if (dragElements.nonEmpty) {
        content --= dragElements
        dragElements = Nil
      }
      if (e.getTransferMode == null) {
        content ++= dragRemoved
        selectedElements.clear()
        for (elem <- dragRemoved) {
          selectedElements += getView(elem)
        }
      }
      dragRemoved = Nil
    }
    var dragElements: List[(Double, Double)] = Nil
    onDragOver = e => if (selectionActive(e)) {
      content --= dragElements
      dragElements = Nil
      val db = e.getDragboard
      if (db.getContentTypes.contains(BeatEditor2.dataformat)) {
        val (offset, snapOffset, list) = db.getContent(BeatEditor2.dataformat).asInstanceOf[(Double, Double, List[((Double, Double), ImmutableTrackElement)])]
        val x = snap(e.getX / pxPerSec() - snapOffset, startPosition, true) + snapOffset - offset
        val fitting = list.forall(elem => clazz.isInstance(elem._2) && elem._1._1 + x >= 0 && content.intersecting(elem._1._1 + x, elem._1._2 + x).isEmpty)
        if (fitting) {
          val insert = list.map(elem => (elem._1._1 + x, elem._1._2 + x, elem._2.toMutable().asInstanceOf[A]))
          content ++= insert
          dragElements = insert.map(elem => (elem._1, elem._2))
          e.acceptTransferModes(TransferMode.Copy, TransferMode.Move)
        }
      }
      e.consume()
    }
    onDragExited = e => {
      if (dragElements.nonEmpty) {
        content --= dragElements
        dragElements = Nil
      }
    }
    onDragDropped = e => if (selectionActive(e)) {
      content --= dragElements
      dragElements = Nil
      val db = e.getDragboard
      val (offset, snapOffset, list) = db.getContent(BeatEditor2.dataformat).asInstanceOf[(Double, Double, List[((Double, Double), ImmutableTrackElement)])]
      val x = snap(e.getX / pxPerSec() - snapOffset, startPosition, true) + snapOffset - offset
      val fitting = list.forall(elem => clazz.isInstance(elem._2) && elem._1._1 + x >= 0 && content.intersecting(elem._1._1 + x, elem._1._2 + x).isEmpty)
      if (fitting) {
        val insert = list.map(elem => (elem._1._1 + x, elem._1._2 + x, elem._2.toMutable().asInstanceOf[A]))
        content ++= insert
        selectedElements.clear()
        for (elem <- insert) {
          selectedElements += getView(elem)
        }
        e.setDropCompleted(true)
      }
      e.consume()
    }
    onMouseDragged = e => {
      if (selectionBox.isEmpty && selectionPos.isDefined) {
        val rect = new Rectangle {
          y = 13 + origin
          height = 24
          stroke = Paint.valueOf("DarkBlue")
          fill = Color.Transparent
        }
        selectionBox = Some(rect)
        children.add(rect)
      }
      selectionBox.foreach { rect =>
        rect.x() = selectionPos.get.min(e.getX)
        rect.width() = (selectionPos.get - e.getX).abs
      }
    }
    onMouseReleased = e => {
      selectionBox.forall { rect =>
        if (!e.isShiftDown && !e.isControlDown) {
          selectedElements.clear()
        }
        val start = rect.x() / pxPerSec()
        val end = (rect.x() + rect.width()) / pxPerSec()
        for (elem <- content.within(start, end)) {
          if (e.isControlDown) {
            selectedElements -= getView(elem)
          } else {
            selectedElements += getView(elem)
          }
        }
        children.remove(rect)
      }
      selectionBox = None
      selectionPos = None
      pressedX = None
    }
    onMouseClicked = e => {
      if (selectionActive(e)) {
        if (e.getClickCount == 1 && e.isStillSincePress) {
          val tmp = content.intersecting(e.getX / pxPerSec(), e.getX / pxPerSec())
          for (elem <- tmp) {
            if (!selectedElements.remove(getView(elem))) {
              selectedElements += getView(elem)
            }
          }
          if (tmp.isEmpty) {
            selectedElements.clear()
          }
        }
        e.consume()
      }
    }

  }

  sealed class TrackElementView[+A <: TrackElement](val element: A, val scaled: DoubleExpression) extends Group {
    val pxPerSec = DoubleProperty(0.0)
    val position = ObjectProperty((0.0, 0.0))
    val duration = Bindings.createDoubleBinding(() => position()._2 - position()._1, position)
    val width = pxPerSec * duration
    val selected = BooleanProperty(false)
  }

  sealed abstract class ResizableElementView[+A <: TrackElement](element: A, scaled: DoubleExpression, context: ObservableIntervalMap[Double, TrackElement]) extends TrackElementView[A](element, scaled) {
    self =>
    def target: Node

    target.onMouseMoved = e => {
      if (e.getX > width.doubleValue() - 5 / scaled.doubleValue()) {
        self.setCursor(Cursor.H_RESIZE)
      } else {
        self.setCursor(Cursor.DEFAULT)
      }
    }
    target.onMouseDragged = e => {
      if (resizeOffset.isDefined) {
        val tmp = position()._1
        val newDuration = minDuration.max((e.getX + resizeOffset.get).max(0.05) / pxPerSec())

        val snapped = snap(tmp + newDuration, 0.0, false)

        if (context.intersecting(tmp, snapped).forall(p => tmp <= p._1 && p._2 <= position()._2)) {
          position() = (tmp, snapped)
        }
        e.consume()
      }
    }

    def minDuration = 0.0

    private var resizeOffset: Option[Double] = None
    target.onMousePressed = e => {
      if (e.isPrimaryButtonDown && e.getX > width.doubleValue() - 5 / scaled.doubleValue()) {
        resizeOffset = Some(width.doubleValue() - e.getX)
        e.consume()
      }
    }
    target.onMouseReleased = e => {
      resizeOffset = None
      if (e.getX < width.doubleValue() - 5 / scaled.doubleValue() || !contains(e.getX, e.getY)) {
        self.setCursor(Cursor.DEFAULT)
      }
    }
  }

  def snap(pos: Double, offset: Double, atStart: Boolean) = Stage.digitDown().flatMap { extraSnaps =>
    mainView.snaps().flatMap { snaps =>
      val (start, end) = if (atStart) {
        (snaps.starting((pos + offset - 10).max(0.0), pos + offset).lastOption.map(_._1 - offset),
          snaps.starting(pos + offset, pos + offset + 10).headOption.map(_._1 - offset))
      } else {
        (snaps.ending((pos + offset - 10).max(0.0), pos + offset).lastOption.map(_._2 - offset),
          snaps.ending(pos + offset, pos + offset + 10).headOption.map(_._2 - offset))
      }
      val points = (start match {
        case Some(s) => end match {
          case Some(e) =>
            Iterator(s) ++ Iterator.range(1, extraSnaps + 1).map {
              i => s + i * (e - s) / (extraSnaps + 1)
            } ++ Iterator(e)
          case None => Iterator(s)
        }
        case None => end.toIterator
      }).filter(x => (x - pos).abs <= 0.1)
      if (points.isEmpty) {
        None
      } else {
        Some(points.minBy(x => (x - pos).abs))
      }
    }
  }.getOrElse(pos)

  final class BeatView(val beat: Beat, val editable: Boolean, scaled: DoubleExpression) extends TrackElementView[Beat](beat, scaled) {
    self =>
    children = Seq(new Rectangle {
      width <== self.width
      height = 10
      y = -5
      if (editable) {
        fill <== when(selected).choose(Color.DarkBlue).otherwise(Color.DarkRed)
      } else {
        fill = Color.DarkGray
      }
    })
  }

  final class BeatsPatternView(val beatsPattern: BeatsPattern, scaled: DoubleExpression, context: ObservableIntervalMap[Double, TrackElement]) extends ResizableElementView[BeatsPattern](beatsPattern, scaled, context) with SelectionContainer[Beat] {
    self =>
    val content = beatsPattern.pattern
    val clazz = classOf[ImmutableBeat]

    override def origin = -26

    val beatsGroup = new Group
    val element2view = mutable.Map[(Double, Double, Beat), TrackElementView[Beat]]()

    override def getView(key: (Double, Double, Beat)): TrackElementView[Beat] = element2view(key)

    override def startPosition = position()._1

    val scaleRect = new Rectangle {
      width <== when(self.width < 5) choose 5 otherwise self.width
      height = 20
      y = -10
      fill = Color.Transparent
    }
    lazy val rectangle = new Rectangle {
      width <== when(self.width < 5) choose 5 otherwise self.width
      height = 20
      y = -10
      fill <== when(selected).choose(Color.Blue).otherwise(Color.LightPink)
    }

    override def target = rectangle

    children = Seq(
      rectangle,
      beatsGroup,
      new Line {
        self =>
        startX <== beatsPattern.patternDuration * pxPerSec
        endX <== beatsPattern.patternDuration * pxPerSec
        startY = -10.0
        endY = 10.0
        strokeWidth = 0.5
        onMousePressed = e => {
          e.consume()
        }
        onMouseMoved = e => {
          self.delegate.setCursor(Cursor.H_RESIZE)
        }
        onMouseDragged = e => {
          val tmp = snap(0.05.max(e.getX / pxPerSec()), position()._1, true)
          if (e.isControlDown) {
            if (beatsPattern.pattern.canMove((0, beatsPattern.patternDuration()), (0, tmp), !_.isInstanceOf[Beat])) {
              beatsPattern.pattern.move((0, beatsPattern.patternDuration()), (0, tmp), !_.isInstanceOf[Beat])
              beatsPattern.patternDuration() = beatsPattern.pattern.lastOption.map(_._2).getOrElse(0.0).max(tmp)
            }
          } else {
            beatsPattern.patternDuration() = beatsPattern.pattern.lastOption.map(_._2).getOrElse(0.0).max(tmp)
          }
          e.consume()
        }
        onMouseReleased = e => {
          self.delegate.setCursor(Cursor.DEFAULT)
          e.consume()
        }
      }
    )

    private def update(): Unit = {
      element2view.clear()
      beatsGroup.children = for ((start, end, b) <- beatsPattern.beats().takeWhile(_._2 < duration.get())) yield {
        val v = new BeatView(b, start < beatsPattern.patternDuration(), scaled)
        v.position() = (start, end)
        v.pxPerSec <== pxPerSec
        v.layoutX <== pxPerSec * start
        element2view.put((start, end, b), v)
        v
      }
    }

    update()
    private val handler: InvalidationListener = _ => update()
    duration.addListener(weak(handler))
    beatsPattern.beats.addListener(weak(handler))

    override def minDuration = beatsPattern.patternDuration()

    override def selectionActive(e: MouseEvent) = e.getX / pxPerSec() <= beatsPattern.patternDuration()

    override def selectionActive(e: DragEvent) = e.getX / pxPerSec() <= beatsPattern.patternDuration()
  }

  final class BPMGroupView(val bpmPattern: BPMPattern, scaled: DoubleExpression, context: ObservableIntervalMap[Double, TrackElement]) extends ResizableElementView[BPMPattern](bpmPattern, scaled, context) {
    self =>
    override def target = this

    private val binding = when(width < 5) choose 5 otherwise width
    val beatsGroup = new Group
    children = Seq(
      new Rectangle {
        width <== binding
        height = 20
        y = -10
        fill <== when(selected).choose(Color.Blue).otherwise(Color.LightPink)
      },
      beatsGroup
    )

    private def update(): Unit = {
      beatsGroup.children = for ((start, end, b) <- bpmPattern.beats().takeWhile(_._2 < duration.get())) yield {
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
    bpmPattern.beats.addListener(weak(handler))

  }

  final class MessageView(val message: Message, scaled: DoubleExpression, context: ObservableIntervalMap[Double, TrackElement]) extends ResizableElementView[Message](message, scaled, context) {
    self =>
    override def target = this

    children = Seq(
      new Rectangle {
        width <== when(self.width < 5) choose 5 otherwise self.width
        height = 20
        y = -10
        fill <== when(selected).choose(Color.Blue).otherwise(Color.LightGreen)
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

  object mainView extends HBox {

    val tracks = new ObservableBuffer[Track]()

    val snaps = ObjectProperty[Option[ObservableIntervalMap[Double, Beat]]](None)

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
                h.snap.selected.onChange { (_, _, b) =>
                  if (b) {
                    track2headerView.values.foreach(h2 => if (h2 != h) {
                      h2.snap.selected = false
                    })
                    snaps() = Some(v.beats)
                  } else {
                    snaps() = None
                  }
                }
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


  object Stage extends PrimaryStage {
    self =>
    title = "Beatmeter Generator"
    val down_0 = BooleanProperty(false)
    val down_1 = BooleanProperty(false)
    val down_2 = BooleanProperty(false)
    val down_3 = BooleanProperty(false)
    val down_4 = BooleanProperty(false)
    val down_5 = BooleanProperty(false)
    val down_6 = BooleanProperty(false)
    val down_7 = BooleanProperty(false)
    val down_8 = BooleanProperty(false)
    val down_9 = BooleanProperty(false)
    val digitDown = Bindings.createObjectBinding(() => {
      if (down_0()) {
        None
      } else if (down_1()) {
        Some(1)
      } else if (down_2()) {
        Some(2)
      } else if (down_3()) {
        Some(3)
      } else if (down_4()) {
        Some(4)
      } else if (down_5()) {
        Some(5)
      } else if (down_6()) {
        Some(6)
      } else if (down_7()) {
        Some(7)
      } else if (down_8()) {
        Some(8)
      } else if (down_9()) {
        Some(9)
      } else {
        Some(0)
      }
    }, down_0, down_1, down_2, down_3, down_4, down_5, down_6, down_7, down_8, down_9)

    scene = new Scene {
      filterEvent(KeyEvent.KeyPressed) { (e: KeyEvent) =>
        if (e.code == KeyCode.Digit0 || e.code == KeyCode.Numpad0) {
          down_0() = true
        } else if (e.code == KeyCode.Digit1 || e.code == KeyCode.Numpad1) {
          down_1() = true
        } else if (e.code == KeyCode.Digit2 || e.code == KeyCode.Numpad2) {
          down_2() = true
        } else if (e.code == KeyCode.Digit3 || e.code == KeyCode.Numpad3) {
          down_3() = true
        } else if (e.code == KeyCode.Digit4 || e.code == KeyCode.Numpad4) {
          down_4() = true
        } else if (e.code == KeyCode.Digit5 || e.code == KeyCode.Numpad5) {
          down_5() = true
        } else if (e.code == KeyCode.Digit6 || e.code == KeyCode.Numpad6) {
          down_6() = true
        } else if (e.code == KeyCode.Digit7 || e.code == KeyCode.Numpad7) {
          down_7() = true
        } else if (e.code == KeyCode.Digit8 || e.code == KeyCode.Numpad8) {
          down_8() = true
        } else if (e.code == KeyCode.Digit9 || e.code == KeyCode.Numpad9) {
          down_9() = true
        }
      }
      filterEvent(KeyEvent.KeyReleased) { (e: KeyEvent) =>
        if (e.code == KeyCode.Digit0 || e.code == KeyCode.Numpad0) {
          down_0() = false
        } else if (e.code == KeyCode.Digit1 || e.code == KeyCode.Numpad1) {
          down_1() = false
        } else if (e.code == KeyCode.Digit2 || e.code == KeyCode.Numpad2) {
          down_2() = false
        } else if (e.code == KeyCode.Digit3 || e.code == KeyCode.Numpad3) {
          down_3() = false
        } else if (e.code == KeyCode.Digit4 || e.code == KeyCode.Numpad4) {
          down_4() = false
        } else if (e.code == KeyCode.Digit5 || e.code == KeyCode.Numpad5) {
          down_5() = false
        } else if (e.code == KeyCode.Digit6 || e.code == KeyCode.Numpad6) {
          down_6() = false
        } else if (e.code == KeyCode.Digit7 || e.code == KeyCode.Numpad7) {
          down_7() = false
        } else if (e.code == KeyCode.Digit8 || e.code == KeyCode.Numpad8) {
          down_8() = false
        } else if (e.code == KeyCode.Digit9 || e.code == KeyCode.Numpad9) {
          down_9() = false
        }
      }
      down_1.onChange(println)
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

  stage = Stage

  def weak[A](listener: ChangeListener[A]): ChangeListener[A] = new WeakChangeListener[A](listener)

  def weak(listener: InvalidationListener): InvalidationListener = new WeakInvalidationListener(listener)

  println("started.")
}

