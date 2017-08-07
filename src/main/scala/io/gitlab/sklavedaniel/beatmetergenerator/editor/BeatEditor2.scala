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
import java.net.URI
import javafx.beans.binding.{DoubleBinding, DoubleExpression}
import javafx.beans.value.{ChangeListener, WeakChangeListener}
import javafx.beans.{InvalidationListener, WeakInvalidationListener, property}
import javafx.geometry.VPos
import javafx.scene.{Cursor, input}
import javax.sound.sampled.{AudioFileFormat, AudioInputStream, AudioSystem}

import io.gitlab.sklavedaniel.beatmetergenerator.utils.{BeatFiles, ObservableIntervalMap}
import io.gitlab.sklavedaniel.beatmetergenerator._
import io.gitlab.sklavedaniel.beatmetergenerator.bpmdetection.WaveletBPMDetection
import io.gitlab.sklavedaniel.beatmetergenerator.editor.AudioPlayer2.BeatInfo
import org.rogach.scallop.ScallopConf

import scala.collection.mutable
import scalafx.Includes._
import scalafx.animation.{Animation, KeyFrame, Timeline}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.binding.{Bindings, ObjectExpression}
import scalafx.beans.property._
import scalafx.collections.ObservableSet.{Add, Remove}
import scalafx.collections.{ObservableBuffer, ObservableSet}
import scalafx.geometry.{Insets, Pos, Side}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{MenuItem, _}
import scalafx.scene.input._
import scalafx.scene.layout._
import scalafx.scene.paint.{Color, Paint}
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.scene.text.{Font, Text}
import scalafx.scene.transform.Scale
import scalafx.scene.{Group, Node, Scene, SnapshotParameters}
import scalafx.stage.FileChooser
import scalafx.stage.FileChooser.ExtensionFilter
import scalafx.util.Duration
import scala.io.Source
import scala.util.{Failure, Success, Try}
import scalafx.scene.image.Image

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


  //new BufferedInputStream(new FileInputStream(audioFile()))
  val player = new AudioPlayer2()

  player.ratio.set(0.8)
  player.rate.set(0.5f)

  class WaveView(initPxPerSec: Double, initHeight: Double, sceneToContextX: Double => Double) extends Group {
    self =>
    val points = ObjectProperty[Option[Array[((Double, Double), Double)]]](None)
    val pxPerSec = DoubleProperty(initPxPerSec)
    val maxVolume = Bindings.createObjectBinding[Option[Double]](() => points().map(_.toIterator.map(v => v._1._1 max v._1._2).max), points)
    val height = DoubleProperty(initHeight)
    val duration = DoubleProperty(0.0)

    private def update() = {
      children.clear()
      children += new Rectangle {
        x = 0.0
        y = 0.0
        width <== pxPerSec * duration
        height <== self.height
        fill = Color.Transparent
      }.delegate
      points().foreach { p =>
        val mv = maxVolume().get
        for (Array((value1, time1), (value2, time2)) <- p.sliding(2)) {
          children += new Line {
            startX <== pxPerSec * time1
            startY <== height * (mv + value1._1) / mv / 2
            endX <== pxPerSec * time2
            endY <== height * (mv + value2._1) / mv / 2
            strokeWidth = 0.5
          }.delegate
        }
        for (Array((value1, time1), (value2, time2)) <- p.sliding(2)) {
          children += new Line {
            startX <== pxPerSec * time1
            startY <== height * (mv - value1._2) / mv / 2
            endX <== pxPerSec * time2
            endY <== height * (mv - value2._2) / mv / 2
            strokeWidth = 0.5
          }.delegate
        }
      }
    }

    update()

    points.onChange(update())
    duration.onChange(update())

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
              fc.title = "Beatmeter Generator: Open audio file"
              fc.getExtensionFilters += new ExtensionFilter("wav audio file (16bit unsigned)", "*.wav")
              Option(fc.showOpenDialog(null)) match {
                case Some(file) =>
                  AudioPlayer2.readData(new BufferedInputStream(new FileInputStream(file))) match {
                    case Success(data) =>
                      track.beat() = Some((file.toURI, data))
                    case Failure(e) =>
                      val alert = new Alert(AlertType.Error) {
                        title = "Beatmeter Generator"
                        headerText = "Could not load file"
                        contentText = e.getLocalizedMessage
                      }
                      alert.showAndWait()
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
          if (tracks().content.head != track) {
            val index = tracks().content.indexOf(track)
            tracks().content.remove(track)
            tracks().content.add(index - 1, track)
          }
        }
      },
      new MenuItem("Move down") {
        onAction = handle {
          if (tracks().content.last != track) {
            val index = tracks().content.indexOf(track)
            tracks().content.remove(track)
            tracks().content.add(index + 1, track)
          }
        }
      },
      new MenuItem("Delete") {
        onAction = handle {
          tracks().content.remove(track)
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
            selected <==> track.snap
          },
          new ToggleButton {
            text = "R"
            tooltip = new Tooltip("record beats to track") {
              font = Font(10)
            }
            font = Font(8)
            focusTraversable = false
            selected <==> track.record
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

  class TrackView(initPxPerSec: Double, val track: Track) extends Group with SelectionContainer[TrackElement] {
    self =>
    val content = track.content
    val clazz = classOf[ImmutableTrackElement]
    val duration = DoubleProperty(0.0)
    override val pxPerSec = DoubleProperty(initPxPerSec)
    val beats = ObservableIntervalMap[Double, Beat]

    override val scaled = {
      val s = new Scale(1.0, 1.0, 0.0, 0.0)
      transforms = Seq(s)
      s.x
    }
    val labelX = DoubleProperty(0.0)

    val elementGroup = new Group {
      layoutY = 25
      layoutX = 0
    }
    val element2view = mutable.Map[(Double, TrackElement), TrackElementView[TrackElement, TrackElement]]()

    override def getView(key: (Double, TrackElement)): TrackElementView[TrackElement, TrackElement] = element2view(key)

    override def startPosition = 0.0

    private def addGenerator(gen: BeatsGenerator, tmp: TrackElementView[TrackElement, TrackElement]): Unit = {
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
          new BeatView[TrackElement](beat, true, self)
        case beatsGroup: BeatsPattern =>
          val tmp = new BeatsPatternView[TrackElement](beatsGroup, self)
          addGenerator(beatsGroup, tmp)
          tmp
        case bpmGroup: BPMPattern =>
          val tmp = new BPMPatternView(bpmGroup, self)
          addGenerator(bpmGroup, tmp)
          tmp
        case message: Message =>
          new MessageView[TrackElement](message, self)
      }
      v.pxPerSec <== pxPerSec
      v.position = (elem._1, elem._2)
      v.position.onChange { (_, old, pos) =>
        if (old != pos && !track.content.contains(pos._1, pos._2)) track.content.move(old, pos)
      }
      v.layoutX <== Bindings.createDoubleBinding(() => pxPerSec() * v.position()._1, pxPerSec, v.position)
      element2view((elem._1, elem._3)) = v
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
            val v = element2view.remove((f._1, b)).get
            element2view += (t._1, b) -> v
            v.position = t
          }
        }
      case (_, ObservableIntervalMap.RemoveChange(l: List[(Double, Double, TrackElement)])) =>
        for (elem <- l) {
          element2view.remove((elem._1, elem._3)).foreach { v =>
            elementGroup.children.remove(v)
            selectedElements.remove(v)
            beats --= beats.intersecting(elem._1, elem._2).map(e => (e._1, e._2))
          }
        }
    }
    track.content.addListener(listener)
    for (elem <- track.content) {
      addElement(elem)
    }

    var contextMenuX = 0.0

    def newResizableElement(elem: TrackElement): Unit = {
      val duration = content.starting(contextMenuX, contextMenuX + 5).headOption.map(x => 0.5.max(x._1 - contextMenuX - 0.5)).getOrElse(5.0)
      if (content.intersecting(contextMenuX, contextMenuX + duration).isEmpty) {
        content ++= Iterator((contextMenuX, contextMenuX + duration, elem))
      }
    }

    val contextMenu: ContextMenu = new ContextMenu(
      new MenuItem("New Beat") {
        onAction = handle {
          val b = new Beat()
          if (content.intersecting(contextMenuX, contextMenuX + 0.05).isEmpty) {
            content ++= Iterator((contextMenuX, contextMenuX + 0.05, b))
          }
        }
      },
      new MenuItem("New BPM Pattern") {
        onAction = handle {
          val b = new BPMPattern()
          b.bpm() = 60
          newResizableElement(b)
        }
      },
      new MenuItem("New Beat Pattern") {
        onAction = handle {
          val b = new BeatsPattern()
          b.pattern ++= Iterator((0.0, 0.05, new Beat()))
          b.patternDuration() = 0.25
          newResizableElement(b)
        }
      },
      new MenuItem("New Message") {
        onAction = handle {
          val b = new Message()
          b.text() = "Hello!"
          newResizableElement(b)
        }
      },
      new MenuItem("Insert") {
        onAction = handle {
          insert(contextMenuX)
        }
      }
    )

    override def mouseClicked(e: MouseEvent) = {
      if (!e.isConsumed && MouseButton.Secondary.equals(e.getButton)) {
        contextMenuX = e.getX / pxPerSec()
        contextMenu.show(self, Side.Bottom, e.getX * scaled(), e.getY - layoutBounds().getHeight)
      } else {
        contextMenu.hide()
      }
      e.consume()
      true
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

    def scaled: DoubleExpression

    def getView(key: (Double, A)): TrackElementView[A, A]

    def origin = 0.0

    def mouseClicked(e: MouseEvent) = false

    def delete(view: TrackElementView[A, A]): Unit = {
      if (view.selected()) {
        content --= selectedElements.map(_.position())
      } else {
        content --= Iterator(view.position())
      }
    }

    def copy(view: TrackElementView[A, A]): Unit = {
      val data: List[(Double, Double, ImmutableTrackElement)] = if (view.selected()) {
        val offset = view.position()._1
        selectedElements.toList.map(elem => (elem.position()._1 - offset, elem.position()._2 - offset, elem.element.toImmutable()))
      } else {
        List((0.0, view.position()._2 - view.position()._1, view.element.toImmutable()))
      }
      val cb = Clipboard.systemClipboard
      val cc = new ClipboardContent()
      cc.put(BeatEditor2.dataformat, data)
      cb.setContent(cc)
    }

    def insert(x: Double): Unit = {
      Clipboard.systemClipboard.content.get(BeatEditor2.dataformat).foreach { data =>
        if (data.asInstanceOf[List[(Double, Double, ImmutableTrackElement)]].forall(elem => clazz.isInstance(elem._3))) {
          val insert = data.asInstanceOf[List[(Double, Double, ImmutableTrackElement)]].map { elem =>
            (elem._1 + x, elem._2 + x, elem._3.toMutable().asInstanceOf[A])
          }
          if (selectionActive(insert.map(_._2).max * pxPerSec()) && insert.forall(elem => content.intersecting(elem._1, elem._2).isEmpty)) {
            content ++= insert
            selectedElements.clear()
            for (elem <- insert) {
              selectedElements += getView((elem._1, elem._3))
            }
          }
        }
      }
    }

    val selectedElements = ObservableSet.empty[TrackElementView[A, A]]
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
      if (selectionActive(e.getX)) {
        pressedX = Some(e.getX)
        val tmp = content.intersecting(e.getX / pxPerSec(), e.getX / pxPerSec())
        if (tmp.isEmpty) {
          selectionPos = Some(e.getX)
        }
        e.consume()
      }
    }
    var dragRemoved: List[(Double, Double, A)] = Nil

    def selectionActive(x: => Double) = true

    onDragDetected = e => {
      if (selectionActive(e.getX)) {
        pressedX.foreach { v =>
          val x = v / pxPerSec()
          val tmp = content.intersecting(x, x)
          if (tmp.nonEmpty) {
            val view = getView(tmp.head._1, tmp.head._3)
            if (!view.selected()) {
              selectedElements.clear()
              selectedElements += view
            }
            val dragMin = selectedElements.map(_.position()._1).min
            val dragWidth = selectedElements.map(_.position()._2).max - dragMin
            val dragOffset = x - selectedElements.map(_.position()._1).min
            val snapOffset = x - tmp.head._1
            val data = (snapOffset, dragOffset, dragWidth, selectedElements.toList.sortBy(_.position()._1).map(x => (x.position()._1 - dragMin, x.position()._2 - dragMin, x.element.toImmutable())))
            if (!e.isControlDown) {
              dragRemoved = selectedElements.toList.map(x => (x.position()._1, x.position()._2, x.element))
              content --= dragRemoved.map(x => (x._1, x._2))
            }
            val cb = startDragAndDrop(if (e.isControlDown) TransferMode.Copy else TransferMode.Move)
            val cc = new input.ClipboardContent()
            cc.put(BeatEditor2.dataformat, data)
            cb.setContent(cc)
          }
        }
        pressedX = None
        e.consume()
      }
    }
    onDragDone = e => {
      if (e.getTransferMode == null) {
        content ++= dragRemoved
        selectedElements.clear()
        for (elem <- dragRemoved) {
          selectedElements += getView((elem._1, elem._3))
        }
      }
      dragRemoved = Nil
    }
    var dragBox: Option[Rectangle] = None
    onDragOver = e => if (selectionActive(e.getX)) {
      val db = e.getDragboard
      if (db.getContentTypes.contains(BeatEditor2.dataformat)) {
        val (snapOffset, dragOffset, dragWidth, list) = db.getContent(BeatEditor2.dataformat).asInstanceOf[(Double, Double, Double, List[(Double, Double, ImmutableTrackElement)])]
        val x = snap(e.getX / pxPerSec() - snapOffset, startPosition, true) + snapOffset - dragOffset
        if (selectionActive((dragWidth + x) * pxPerSec()) && x >= 0) {
          if (dragBox.isEmpty) {
            val r = new Rectangle {
              y = 13 + origin
              height = 24
              width = dragWidth * pxPerSec()
            }
            children += r
            dragBox = Some(r)
          }
          val fitting = list.forall(elem => content.intersecting(elem._1 + x, elem._2 + x).isEmpty)
          dragBox.get.x = x * pxPerSec()
          if (fitting) {
            dragBox.get.fill = Color.DarkBlue.opacity(0.5)
            e.acceptTransferModes(TransferMode.Copy, TransferMode.Move)
          } else {
            dragBox.get.fill = Color.DarkRed.opacity(0.5)
          }
        }
      }
      e.consume()
    }
    onDragExited = e => {
      dragBox.foreach(children.remove)
      dragBox = None
    }
    onDragDropped = e => if (selectionActive(e.getX)) {
      dragBox.foreach(children.remove)
      dragBox = None
      val db = e.getDragboard
      val (snapOffset, dragOffset, dragWidth, list) = db.getContent(BeatEditor2.dataformat).asInstanceOf[(Double, Double, Double, List[(Double, Double, ImmutableTrackElement)])]
      val x = snap(e.getX / pxPerSec() - snapOffset, startPosition, true) + snapOffset - dragOffset
      val fitting = selectionActive((dragWidth + x) * pxPerSec()) && x >= 0 && list.forall(elem => clazz.isInstance(elem._3) && content.intersecting(elem._1 + x, elem._2 + x).isEmpty)
      if (fitting) {
        val insert = list.map(elem => (elem._1 + x, elem._2 + x, elem._3.toMutable().asInstanceOf[A]))
        content ++= insert
        selectedElements.clear()
        for (elem <- insert) {
          selectedElements += getView((elem._1, elem._3))
        }
        e.setDropCompleted(true)
      }
      e.consume()
      dragRemoved = Nil
    }
    onMouseDragged = e => {
      if (selectionBox.isEmpty && selectionPos.isDefined) {
        val rect = new Rectangle {
          y = 13 + origin
          height = 24
          stroke = Color.DarkBlue
          fill = Color.Transparent
        }
        selectionBox = Some(rect)
        children.add(rect)
      }
      selectionBox.foreach { rect =>
        val x = e.getX.max(0.0)
        rect.x() = selectionPos.get.min(x)
        rect.width() = (selectionPos.get - x).abs
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
            selectedElements -= getView((elem._1, elem._3))
          } else {
            selectedElements += getView((elem._1, elem._3))
          }
        }
        children.remove(rect)
      }
      selectionBox = None
      selectionPos = None
      pressedX = None
    }
    onMouseClicked = e => {
      if (MouseButton.Primary.equals(e.getButton) && selectionActive(e.getX)) {
        if (e.getClickCount == 1 && e.isStillSincePress) {
          val tmp = content.intersecting(e.getX / pxPerSec(), e.getX / pxPerSec())
          for (elem <- tmp) {
            if (!selectedElements.remove(getView((elem._1, elem._3)))) {
              selectedElements += getView((elem._1, elem._3))
            }
          }
          if (tmp.isEmpty) {
            selectedElements.clear()
          }
        }
        e.consume()
      }
      mouseClicked(e)
    }

  }

  sealed class TrackElementView[+A <: TrackElement, B <: TrackElement](val element: A, val context: SelectionContainer[B]) extends Group {
    val pxPerSec = DoubleProperty(0.0)
    private val position_ = ReadOnlyObjectWrapper((0.0, 0.0))

    def position = position_.readOnlyProperty

    def position_=(value: (Double, Double)): Unit = {
      position_() = value
    }

    val duration = Bindings.createDoubleBinding(() => position()._2 - position()._1, position)
    val width = pxPerSec * duration
    val selected = BooleanProperty(false)
  }

  sealed abstract class ResizableElementView[A <: TrackElement, B <: TrackElement](element: A, context: SelectionContainer[B]) extends TrackElementView[A, B](element, context) {
    self =>
    def target: Node

    target.onMouseMoved = e => {
      if (e.getX > width.doubleValue() - 5 / context.scaled.doubleValue()) {
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

        resizeBox.foreach { r =>
          r.width = e.getX + resizeOffset.get
          if (context.content.intersecting(tmp, snapped).forall(p => tmp <= p._1 && p._2 <= position()._2)) {
            r.fill = Color.DarkBlue.opacity(0.5)
          } else {
            r.fill = Color.DarkRed.opacity(0.5)
          }
        }
        e.consume()
      }
    }

    def minDuration = 0.0

    private var resizeOffset: Option[Double] = None
    private var resizeBox: Option[Rectangle] = None
    target.onMousePressed = e => {
      if (e.isPrimaryButtonDown && e.getX > width.doubleValue() - 5 / context.scaled.doubleValue()) {
        resizeOffset = Some(width.doubleValue() - e.getX)
        val r = new Rectangle {
          x = 0.0
          y = -10.0
          height = 20.0
          width = self.width.doubleValue()
          fill = Color.DarkBlue.opacity(0.5)
        }
        children += r
        resizeBox = Some(r)
        e.consume()
      } else if (e.isPrimaryButtonDown && e.getX > width.doubleValue() - 8 / context.scaled.doubleValue()) {
        e.consume()
      }
    }
    target.onMouseReleased = e => {
      if (resizeOffset.isDefined) {
        val tmp = position()._1
        val newDuration = minDuration.max((e.getX + resizeOffset.get).max(0.05) / pxPerSec())

        val snapped = snap(tmp + newDuration, 0.0, false)

        if (context.content.intersecting(tmp, snapped).forall(p => tmp <= p._1 && p._2 <= position()._2)) {
          position = (tmp, snapped)
        }
        resizeBox.foreach(children.remove)
        e.consume()
      }
      resizeOffset = None
      if (e.getX < width.doubleValue() - 5 / context.scaled.doubleValue() || !contains(e.getX, e.getY)) {
        self.setCursor(Cursor.DEFAULT)
      }
    }
  }

  def snap(pos: Double, offset: Double, atStart: Boolean) = Stage.digitDown().flatMap {
    extraSnaps =>
      mainView().snaps().flatMap {
        snaps =>
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

  final class BeatView[B >: Beat <: TrackElement](val beat: Beat, val editable: Boolean, context: SelectionContainer[B]) extends TrackElementView[Beat, B](beat, context) {
    self =>

    override def position_=(value: (Double, Double)): Unit = {
      super.position_=(value._1, value._1 + 0.05)
    }

    val highlight = BooleanProperty(false)
    children = Seq(new Rectangle {
      width <== self.width
      height = 10
      y = -5
      if (editable) {
        fill <== when(selected).choose(Color.DarkBlue).otherwise(when(highlight || beat.highlight).choose(Color.OrangeRed).otherwise(Color.DarkRed))
      } else {
        fill <== when(highlight || beat.highlight).choose(Color.Black).otherwise(Color.DarkGray)
      }
    })
    val contextMenu = new ContextMenu(
      new MenuItem("Copy") {
        onAction = handle {
          context.copy(self)
        }
      },
      new MenuItem("Delete") {
        onAction = handle {
          context.delete(self)
        }
      },
      new CheckMenuItem("Highlight") {
        selected <==> beat.highlight
      }
    )
    onMouseClicked = e => {
      if (editable && MouseButton.Secondary.equals(e.getButton)) {
        contextMenu.show(self, Side.Bottom, e.getX * context.scaled.doubleValue(), e.getY - layoutBounds().getHeight)
        e.consume()
      } else {
        contextMenu.hide()
      }
    }
  }

  final class BeatsPatternView[B >: BeatsPattern <: TrackElement](val beatsPattern: BeatsPattern, context: SelectionContainer[B]) extends ResizableElementView[BeatsPattern, B](beatsPattern, context) with SelectionContainer[Beat] {
    self =>
    val content = beatsPattern.pattern
    val clazz = classOf[ImmutableBeat]

    override def scaled = context.scaled

    override def origin = -26

    val beatsGroup = new Group
    val element2view = mutable.Map[(Double, Beat), TrackElementView[Beat, Beat]]()

    override def getView(key: (Double, Beat)): TrackElementView[Beat, Beat] = element2view(key)

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
        self2 =>
        startX <== beatsPattern.patternDuration * pxPerSec
        endX <== beatsPattern.patternDuration * pxPerSec
        startY = -10.0
        endY = 10.0
        strokeWidth = 0.5
        private var dragBeatsPattern: Option[ImmutableBeatsPattern] = None

        private var line: Option[Line] = None
        onMousePressed = e => {
          dragBeatsPattern = Some(beatsPattern.toImmutable())
          val l = new Line {
            startY = -10.0
            endY = 10.0
            strokeWidth = 0.5
            startX = beatsPattern.patternDuration() * pxPerSec()
            endX = beatsPattern.patternDuration() * pxPerSec()
            stroke = Color.DarkBlue.opacity(0.5)
          }
          self.children += l
          line = Some(l)
          e.consume()
        }
        onMouseMoved = e => {
          self2.delegate.setCursor(Cursor.H_RESIZE)
        }
        onMouseDragged = e => {
          val tmp = snap(0.05.max(e.getX / pxPerSec()), position()._1, true)
          line.get.startX = tmp * pxPerSec()
          line.get.endX = tmp * pxPerSec()
          if (e.isControlDown) {
            if (beatsPattern.pattern.canMove((0, beatsPattern.patternDuration()), (0, tmp))) {
              line.get.stroke = Color.DarkBlue
            } else {
              line.get.stroke = Color.DarkRed
            }
          } else if (tmp >= beatsPattern.pattern.lastOption.map(_._2).getOrElse(0.0)) {
            line.get.stroke = Color.DarkBlue
          } else {
            line.get.stroke = Color.DarkRed
          }
          e.consume()
        }
        onMouseReleased = e => {
          line.foreach(self.children.remove)
          line = None
          val tmp = snap(0.05.max(e.getX / pxPerSec()), position()._1, true)
          if (e.isControlDown) {
            if (beatsPattern.pattern.canMove((0, beatsPattern.patternDuration()), (0, tmp))) {
              beatsPattern.pattern.move((0, beatsPattern.patternDuration()), (0, tmp))
              beatsPattern.patternDuration() = beatsPattern.pattern.lastOption.map(_._2).getOrElse(0.0).max(tmp)
            } else {
              beatsPattern.patternDuration() = dragBeatsPattern.get.patternDuration
              beatsPattern.pattern.clear()
              beatsPattern.pattern ++= dragBeatsPattern.get.toMutable().pattern
            }
          } else {
            beatsPattern.patternDuration() = beatsPattern.pattern.lastOption.map(_._2).getOrElse(0.0).max(tmp)
          }
          self2.delegate.setCursor(Cursor.DEFAULT)
          e.consume()
        }
      }
    )

    private def update(): Unit = {
      element2view.clear()
      if (beatsPattern.pattern.nonEmpty) {
        beatsGroup.children = beatsPattern.pattern.headOption.map { elem =>
          val v = new BeatView[Beat](elem._3, true, self)
          v.position = (elem._1, elem._2)
          v.pxPerSec <== pxPerSec
          v.layoutX <== pxPerSec * elem._1
          element2view.put((elem._1, elem._3), v)
          v.highlight <== beatsPattern.highlightFirst
          v
        }.toIterable ++ (for ((start, end, b) <- beatsPattern.pattern.tail) yield {
          val v = new BeatView[Beat](b, true, self)
          v.position = (start, end)
          v.pxPerSec <== pxPerSec
          v.layoutX <== pxPerSec * start
          element2view.put((start, b), v)
          v
        }) ++ (for ((start, end, b) <- beatsPattern.beats().drop(beatsPattern.pattern.size).takeWhile(_._2 < duration.get())) yield {
          val v = new BeatView[Beat](b, false, self)
          v.position = (start, end)
          v.pxPerSec <== pxPerSec
          v.layoutX <== pxPerSec * start
          element2view.put((start, b), v)
          v
        })
      } else {
        beatsGroup.children.clear()
      }
    }

    update()
    private val handler: InvalidationListener = _ => update()
    duration.addListener(weak(handler))
    beatsPattern.beats.addListener(weak(handler))

    override def minDuration = beatsPattern.patternDuration()

    override def selectionActive(x: => Double) = x / pxPerSec() <= beatsPattern.patternDuration()


    var contextMenuX = 0.0
    val contextMenu = new ContextMenu(
      new MenuItem("New Beat") {
        onAction = handle {
          val b = new Beat()
          if (content.intersecting(contextMenuX, contextMenuX + 0.05).isEmpty) {
            content ++= Iterator((contextMenuX, contextMenuX + 0.05, b))
          }
        }
      },
      new MenuItem("Copy") {
        onAction = handle {
          context.copy(self)
        }
      },
      new MenuItem("Insert") {
        onAction = handle {
          insert(contextMenuX)
        }
      },
      new MenuItem("Delete") {
        onAction = handle {
          context.delete(self)
        }
      },
      new CheckMenuItem("Highlight First") {
        selected <==> beatsPattern.highlightFirst
      }
    )

    override def mouseClicked(e: MouseEvent) = {
      if (!e.isConsumed && MouseButton.Secondary.equals(e.getButton)) {
        contextMenuX = e.getX / pxPerSec()
        contextMenu.show(self, Side.Bottom, e.getX * context.scaled.doubleValue(), e.getY - layoutBounds().getHeight)
      } else {
        contextMenu.hide()
      }
      e.consume()
      true
    }
  }

  final class BPMPatternView(val bpmPattern: BPMPattern, context: SelectionContainer[TrackElement]) extends ResizableElementView[BPMPattern, TrackElement](bpmPattern, context) {
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
      beatsGroup.children = bpmPattern.beats().headOption.map { elem =>
        val v = new BeatView[TrackElement](elem._3, false, context)
        v.pxPerSec <== pxPerSec
        v.position = (elem._1, elem._2)
        v.layoutX <== pxPerSec * elem._1
        v.highlight <== bpmPattern.highlightFirst
        v
      }.toIterable ++ (for ((start, end, b) <- bpmPattern.beats().tail.takeWhile(_._2 < duration.get())) yield {
        val v = new BeatView[TrackElement](b, false, context)
        v.pxPerSec <== pxPerSec
        v.position = (start, end)
        v.layoutX <== pxPerSec * start
        v
      })
    }

    update()
    private val handler: InvalidationListener = _ => update()
    duration.addListener(weak(handler))
    bpmPattern.beats.addListener(weak(handler))

    val bpmSpinner = new Spinner[Double](1.0, 300.0, bpmPattern.bpm(), 1.0) {
      editable = true

      private def commitEditorText(): Unit = {
        if (editable()) {
          val text = editor().text()
          if (valueFactory() != null) {
            val converter = valueFactory().getConverter
            if (converter != null) {
              valueFactory().value = converter.fromString(text)
            }
          }
        }
      }

      focused.onChange { (_, _, nv) =>
        if (!nv) {
          commitEditorText()
        }
      }
    }
    bpmSpinner.value.onChange { (_, old, now) =>
      if (old != now) {
        bpmPattern.bpm() = now
      }
    }
    bpmPattern.bpm.onChange { (_, old, now) =>
      if (old.doubleValue() != now.doubleValue()) {
        bpmSpinner.valueFactory().value = now.doubleValue()
      }
    }
    val contextMenu = new ContextMenu(
      new CustomMenuItem(bpmSpinner, false),
      new MenuItem("Copy") {
        onAction = handle {
          context.copy(self)
        }
      },
      new MenuItem("Delete") {
        onAction = handle {
          context.delete(self)
        }
      },
      new CheckMenuItem("Highlight First") {
        selected <==> bpmPattern.highlightFirst
      },
      new CheckMenuItem("Detect BPM") {
        onAction = handle {
          player.audio().foreach { data =>
            val detection = new WaveletBPMDetection(WaveletBPMDetection.Daubechies4, 17)
            val dataSlice = data.slice((position()._1 * AudioPlayer2.format.getFrameRate * 2).toInt, (position()._2 * AudioPlayer2.format.getFrameRate * 2).toInt + 1)
            val monoData = dataSlice.toIterator.grouped(AudioPlayer2.format.getChannels).map(_.map(_.toDouble).sum / Short.MaxValue).toArray
            val (bpm, bpms) = detection.detect(monoData, AudioPlayer2.format.getSampleRate, Some((t, d) => {
              println(f"$d%.3f in window starting at $t%.3f")
            }))
            bpmPattern.bpm() = bpm
          }
        }
      }
    )
    onMouseClicked = e => {
      if (MouseButton.Secondary.equals(e.getButton)) {
        contextMenu.show(self, Side.Bottom, e.getX * context.scaled.doubleValue(), e.getY - layoutBounds().getHeight)
        e.consume()
      } else {
        contextMenu.hide()
      }
    }
  }

  final class MessageView[B >: Message <: TrackElement](val message: Message, context: SelectionContainer[B]) extends ResizableElementView[Message, B](message, context) {
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
    val contextMenu = new ContextMenu(
      new CustomMenuItem(new TextField() {
        text <==> message.text
        editable = true

      }, false),
      new MenuItem("Copy") {
        onAction = handle {
          context.copy(self)
        }
      },
      new MenuItem("Delete") {
        onAction = handle {
          context.delete(self)
        }
      }
    )
    onMouseClicked = e => {
      if (MouseButton.Secondary.equals(e.getButton)) {
        contextMenu.show(self, Side.Bottom, e.getX * context.scaled.doubleValue(), e.getY - layoutBounds().getHeight)
        e.consume()
      } else {
        contextMenu.hide()
      }
    }
  }

  val beats = conf.beats.toOption.map(BeatFiles.load(_).map(t => (t, t + 0.05, new Beat()))).getOrElse(Seq[(Double, Double, Beat)]())


  val tracks = ObjectProperty(new Tracks())
  val audioListener: InvalidationListener = _ => {
    player.audio() = tracks().audio().map(_._2)
  }
  tracks.onChange { (_, old, current) =>
    player.audio() = tracks().audio().map(_._2)
    old.audio.removeListener(audioListener)
    current.audio.addListener(audioListener)
  }
  tracks().audio.addListener(audioListener)

  var mainView = Bindings.createObjectBinding(() => new MainView(tracks()), tracks)

  class MainView(val tracks: Tracks) extends HBox {

    private val tracksDuration_ = ReadOnlyDoubleWrapper(0.0)
    val tracksDuration = tracksDuration_.readOnlyProperty

    private def calcTracksDuration(): Unit = {
      tracksDuration_() = (Iterator(0.0) ++ (for {
        track <- tracks.content
        (_, d, _) <- track.content.lastOption
      } yield d)).max
    }

    val snaps = ObjectProperty[Option[ObservableIntervalMap[Double, Beat]]](None)
    val record = ObjectProperty[Option[Track]](None)

    val headerBox = new VBox {
      spacing = 5
    }

    object scrollPane extends ScrollPane {
      self =>
      override def requestFocus() {

      }

      focusTraversable = false

      val sceeneToContextX = (x: Double) => sceneToLocal(x, 0.0).getX + scrollX
      val waveView = new WaveView(20, 200, sceeneToContextX) {
        points <== player.maxima
        duration <== Bindings.createDoubleBinding(() => 10.0.max(player.audioDuration().max(tracksDuration())), player.duration, tracksDuration)
      }

      val scrollTimeline = new Timeline {
        keyFrames = KeyFrame(time = Duration(50), onFinished = _ => {
          scrollX = 0.0.max(scrollX + scrollDelta)
        })
        cycleCount = Animation.Indefinite
      }
      var scrollDelta: Double = 0.0

      val tracksBox = new VBox {
        spacing = 5
      }

      private val track2view = mutable.Map[Track, TrackView]()
      private val track2headerView = mutable.Map[Track, TrackHeaderView]()

      private val tracksListener: InvalidationListener = _ => {
        calcTracksDuration()
      }

      private def addTrack(i: Int, t: Track): Unit = {
        t.content.addListener(tracksListener)
        val v = new TrackView(20, t) {
          scaled <== waveView.scale
          duration <== Bindings.createDoubleBinding(() => 10.0.max(player.audioDuration().max(tracksDuration())), player.duration, tracksDuration)
        }
        val h = new TrackHeaderView(t) {
          width <== headerBox.width
        }
        track2view(t) = v
        track2headerView(t) = h
        tracksBox.children.add(i, v)
        headerBox.children.add(i, h)
        val info = new BeatInfo()
        info.beat <== Bindings.createObjectBinding(() => {
          t.beat().map(_._2).getOrElse(AudioPlayer2.defaultBeat)
        }, t.beat)
        player.beats.add(i, (h.track.play, info, v.beats))
        h.track.snap.onChange { (_, _, b) =>
          if (b) {
            track2headerView.values.foreach(h2 => if (h2 != h) {
              h2.track.snap() = false
            })
            snaps() = Some(v.beats)
          } else {
            snaps() = None
          }
        }
        h.track.record.onChange { (_, _, b) =>
          if (b) {
            track2headerView.values.foreach(h2 => if (h2 != h) {
              h2.track.record() = false
            })
            record() = Some(h.track)
          } else {
            record() = None
          }
        }
      }

      tracks.content.onChange { (_, cs) =>
        calcTracksDuration()
        for (c <- cs) {
          c match {
            case ObservableBuffer.Add(i, ts) =>
              for (t <- ts) {
                addTrack(i, t)
              }
            case ObservableBuffer.Remove(i, ts) =>
              for (t <- ts) {
                t.content.removeListener(tracksListener)
                track2view.remove(t).foreach(tracksBox.children.remove)
                track2headerView.remove(t).foreach(headerBox.children.remove)
                player.beats.remove(i)
              }
            case _ => assert(false)
          }
        }
      }

      for ((t, i) <- tracks.content.zipWithIndex) {
        addTrack(i, t)
      }
      calcTracksDuration()

      val box = new VBox {
        spacing = 5
        children = Seq(
          waveView,
          tracksBox
        )
        filterEvent(DragEvent.DragOver) { (e: DragEvent) =>
          if (e.getX < scrollX + 10) {
            scrollDelta = e.getX - scrollX - 10
            if (!Animation.Status.Running.equals(scrollTimeline.status())) {
              scrollTimeline.play()
            }
          } else if (e.getX > scrollX + scrollPane.viewportBounds().getWidth - 10) {
            scrollDelta = e.getX - scrollX - scrollPane.viewportBounds().getWidth + 10
            if (!Animation.Status.Running.equals(scrollTimeline.status())) {
              scrollTimeline.play()
            }
          } else if (Animation.Status.Running.equals(scrollTimeline.status())) {
            scrollTimeline.stop()
          }
        }
        filterEvent(MouseEvent.MouseDragged) { (e: MouseEvent) =>
          if (e.getX < scrollX + 10) {
            scrollDelta = e.getX - scrollX - 10
            if (!Animation.Status.Running.equals(scrollTimeline.status())) {
              scrollTimeline.play()
            }
          } else if (e.getX > scrollX + scrollPane.viewportBounds().getWidth - 10) {
            scrollDelta = e.getX - scrollX - scrollPane.viewportBounds().getWidth + 10
            if (!Animation.Status.Running.equals(scrollTimeline.status())) {
              scrollTimeline.play()
            }
          } else if (Animation.Status.Running.equals(scrollTimeline.status())) {
            scrollTimeline.stop()
          }
        }
        filterEvent(MouseEvent.MouseReleased) { (e: MouseEvent) =>
          if (Animation.Status.Running.equals(scrollTimeline.status())) {
            scrollTimeline.stop()
          }
        }
      }

      val positionLineView = new PositionLineView(20, 10, sceeneToContextX) {
        position <==> player.position
        scale <== waveView.scale
        duration <== Bindings.createDoubleBinding(() => 10.0.max(player.audioDuration().max(tracksDuration())), player.duration, tracksDuration)
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

      filterEvent(ScrollEvent.Scroll) { (e: ScrollEvent) =>
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
      }

      val minX = DoubleProperty(0.1)
      val maxX = DoubleProperty(0.9)

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

      root = new BorderPane {
        prefWidth = 800
        top = new MenuBar {
          menus = Seq(
            new Menu("File") {
              items = Seq(
                new MenuItem("Open") {
                  onAction = handle {
                    val fc = new FileChooser()
                    fc.title = "Beatmeter Generator: Open"
                    Option(fc.showOpenDialog(window())) match {
                      case Some(file) =>
                        try {
                          JsonSerialization.load(Source.fromFile(file, "utf-8").mkString) match {
                            case Success(ts) => {
                              ts.toMutable(file.getParentFile.toURI, uri => {
                                Try(uri.toURL().openStream()).flatMap(in => AudioPlayer2.readData(new BufferedInputStream(in)))
                              }) match {
                                case Success(t) =>
                                  tracks() = t
                                case Failure(e) =>
                                  val alert = new Alert(AlertType.Error) {
                                    title = "Beatmeter Generator"
                                    headerText = "Could not load file"
                                    contentText = e.getLocalizedMessage
                                  }
                                  alert.showAndWait()
                              }
                            }
                            case Failure(e) =>
                              val alert = new Alert(AlertType.Error) {
                                title = "Beatmeter Generator"
                                headerText = "Could not load file"
                                contentText = e.getLocalizedMessage
                              }
                              alert.showAndWait()
                          }
                        } catch {
                          case e: Exception =>
                            val alert = new Alert(AlertType.Error) {
                              title = "Beatmeter Generator"
                              headerText = "Could not open file"
                              contentText = e.getLocalizedMessage
                            }
                            alert.showAndWait()
                        }
                      case None =>
                    }
                  }
                },
                new MenuItem("Save") {
                  onAction = handle {
                    val fc = new FileChooser()
                    fc.title = "Beatmeter Generator: Save"
                    Option(fc.showSaveDialog(window())) match {
                      case Some(file) =>
                        val s = JsonSerialization.save(tracks().toImmutable(file.getParentFile.toURI))
                        val r = for (out <- resource.managed(new OutputStreamWriter(new FileOutputStream(file), "utf-8"))) yield {
                          out.write(s)
                        }
                        r.tried match {
                          case Success(_) =>
                          case Failure(e) =>
                            val alert = new Alert(AlertType.Error) {
                              title = "Beatmeter Generator"
                              headerText = "Could not save file"
                              contentText = e.getLocalizedMessage
                            }
                            alert.showAndWait()
                        }
                      case None =>
                    }
                  }
                },
                new MenuItem("Load Audio") {
                  onAction = handle {
                    val fc = new FileChooser()
                    fc.title = "Beatmeter Generator: Open audio file"
                    fc.getExtensionFilters += new ExtensionFilter("wav audio file (16bit unsigned)", "*.wav")
                    Option(fc.showOpenDialog(window())) match {
                      case Some(file) =>
                        AudioPlayer2.readData(new BufferedInputStream(new FileInputStream(file))) match {
                          case Success(data) =>
                            tracks().audio() = Some((file.toURI, data))
                          case Failure(e) =>
                            val alert = new Alert(AlertType.Error) {
                              title = "Beatmeter Generator"
                              headerText = "Could not load file"
                              contentText = e.getLocalizedMessage
                            }
                            alert.showAndWait()
                        }
                      case None =>
                    }
                  }
                }
              )
            },
            new Menu("Edit") {
              items = Seq(
                new MenuItem("New Track") {
                  onAction = handle {
                    tracks().content += new Track()
                  }
                },
                new MenuItem("Undo"),
                new MenuItem("Redo")
              )
            },
            new Menu("Tools") {
              items = Seq(
                new MenuItem("Generate Audio") {
                  onAction = handle {
                    val fc = new FileChooser()
                    fc.title = "Beatmeter Generator: Generate Audio"
                    Option(fc.showSaveDialog(window())) match {
                      case Some(file) =>
                        try {
                          val (is, count) = player.generateStream()
                          val ais = new AudioInputStream(is, AudioPlayer2.format, count)
                          try {
                            AudioSystem.write(ais, AudioFileFormat.Type.WAVE, file)
                          } finally {
                            ais.close()
                          }
                          println("finished")
                        } catch {
                          case e: Throwable =>
                            val alert = new Alert(AlertType.Error) {
                              title = "Beatmeter Generator"
                              headerText = "Could not save file"
                              contentText = e.getLocalizedMessage
                            }
                            alert.showAndWait()
                        }
                      case None =>
                    }


                  }
                },
                new MenuItem("Generate Beatmeter"),
                new MenuItem("Beatmeter Settings")
              )
            }
          )
        }
        center = mainView()
        mainView.onChange { (_, _, v) =>
          center() = v
        }
        bottom = new ToolBar {
          padding = Insets(0, 0, 0, 0)
          items = Seq(
            new ToggleButton("play") {
              selected <==> player.playing
              focusTraversable = false
            },
            new Button("beat") {
              tooltip = Tooltip("Insert beat at current position")
              focusTraversable = false
              onAction = handle {
                mainView().record().foreach { track =>
                  val x = player.position()._1
                  if (track.content.intersecting(x, x + 0.05).isEmpty) {
                    track.content ++= Iterator((x, x + 0.05, new Beat()))
                  }
                }
              }
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
                new Text("Position"),
                new Text {
                  text <== Bindings.createStringBinding(() => formatTime(player.position()._1), player.position)
                }.delegate
              )
              addRow(1,
                new Text("Duration"),
                new Text {
                  text <== Bindings.createStringBinding(() => formatTime(player.duration()), player.duration)
                }.delegate
              )
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

  mainView().tracks.content.append(new Track() {
    title() = "very long title"
    content ++= beats
  })
  mainView().tracks.content.append(new Track() {
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

  def formatTime(time: Double) = {
    val ms = (time * 1000).round
    val minutes = ms / (1000 * 60)
    val seconds = ms % (1000 * 60) / 1000
    val milis = ms % 1000
    f"$minutes%02d:$seconds%02d.$milis%03d"
  }
}

