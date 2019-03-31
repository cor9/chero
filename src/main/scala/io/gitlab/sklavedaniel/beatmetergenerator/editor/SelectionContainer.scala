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

import io.gitlab.sklavedaniel.beatmetergenerator.utils.ImmutableTracks_V0_2_0.ImmutableTrackElement
import io.gitlab.sklavedaniel.beatmetergenerator.utils.{ObservableIntervalMap, TrackElement, UndoManager}
import javafx.beans.binding.DoubleExpression
import javafx.scene.input
import scalafx.Includes._
import scalafx.beans.binding.{Bindings, BooleanBinding}
import scalafx.beans.property.{DoubleProperty, ObjectProperty}
import scalafx.collections.ObservableSet
import scalafx.collections.ObservableSet.{Add, Remove}
import scalafx.scene.Group
import scalafx.scene.input._
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

trait SelectionContainer[A <: TrackElement] {
  self: Group =>
  def clazz: Class[_]

  def selectionContainer: ObjectProperty[Option[SelectionContainer[_]]]

  def undoManager: UndoManager

  def startPosition: Double

  def pxPerSec: DoubleProperty

  def content: ObservableIntervalMap[Double, A]

  def scaled: DoubleExpression

  def getView(key: (Double, A)): TrackElementView[A, A]

  def origin = 0.0

  def mouseClicked(e: MouseEvent) = false

  def snap(pos: Double, offset: Double, atStart: Boolean): Double

  def propagateDragOver()

  def delete(view: TrackElementView[A, A]): Unit = {
    if (view.selected()) {
      content --= selectedElements.map(_.position())
    } else {
      content --= Iterator(view.position())
    }
  }

  def deleteSelectedElemts(): Unit = {
    content --= selectedElements.map(_.position())
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
    cc.put(BeatEditor.dataformat, data)
    cb.setContent(cc)
  }

  def copySlected(): Unit = {
    if (selectedElements.nonEmpty) {
      val offset = selectedElements.toList.map(_.position()._1).min
      val data: List[(Double, Double, ImmutableTrackElement)] = selectedElements.toList.map(
        elem => (elem.position()._1 - offset, elem.position()._2 - offset, elem.element.toImmutable()))
      val cb = Clipboard.systemClipboard
      val cc = new ClipboardContent()
      cc.put(BeatEditor.dataformat, data)
      cb.setContent(cc)
    }
  }


  def insert(x: Double): Unit = {
    Clipboard.systemClipboard.content.get(BeatEditor.dataformat).foreach { data =>
      if (data.asInstanceOf[List[(Double, Double, ImmutableTrackElement)]].forall(elem => clazz.isInstance(elem._3))) {
        val insert = data.asInstanceOf[List[(Double, Double, ImmutableTrackElement)]].map { elem =>
          (elem._1 + x, elem._2 + x, elem._3.toMutable(Some(undoManager)).asInstanceOf[A])
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
  val hasSelectedElements = Bindings.createBooleanBinding(() => selectedElements.nonEmpty, selectedElements)
  hasSelectedElements.onChange { (_, _, v) =>
    if (v) {
      selectionContainer() = Some(this)
    } else if (selectionContainer().contains(this)) {
      selectionContainer() = None

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
            undoManager.startGroup()
            dragRemoved = selectedElements.toList.map(x => (x.position()._1, x.position()._2, x.element))
            content --= dragRemoved.map(x => (x._1, x._2))
          }
          val cb = startDragAndDrop(if (e.isControlDown) TransferMode.Copy else TransferMode.Move)
          val cc = new input.ClipboardContent()
          cc.put(BeatEditor.dataformat, data)
          cb.setContent(cc)
        }
      }
      pressedX = None
      e.consume()
    }
  }
  onDragDone = e => {
    if (dragRemoved.nonEmpty) {
      undoManager.endGroup()
      if (e.getTransferMode == null) {
        undoManager.undoAction()
        selectedElements.clear()
        for (elem <- dragRemoved) {
          selectedElements += getView((elem._1, elem._3))
        }
      }
      dragRemoved = Nil
    }
    e.consume()
  }
  var dragBox: Option[Rectangle] = None
  onDragOver = e => {
    propagateDragOver()
    if (selectionActive(e.getX)) {
      val db = e.getDragboard
      if (db.getContentTypes.contains(BeatEditor.dataformat)) {
        val (snapOffset, dragOffset, dragWidth, list) = db.getContent(BeatEditor.dataformat).asInstanceOf[(Double, Double, Double, List[(Double, Double, ImmutableTrackElement)])]
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
        } else {
          dragExit()
        }
      } else {
        dragExit()
      }
    } else {
      dragExit()
    }
    e.consume()
  }

  onDragExited = e => {
    dragExit()
  }

  def dragExit(): Unit = {
    dragBox.foreach(children.remove)
    dragBox = None
  }

  onDragDropped = e => {
    if (selectionActive(e.getX)) {
      dragBox.foreach(children.remove)
      dragBox = None
      val db = e.getDragboard
      val (snapOffset, dragOffset, dragWidth, list) = db.getContent(BeatEditor.dataformat).asInstanceOf[(Double, Double, Double, List[(Double, Double, ImmutableTrackElement)])]
      val x = snap(e.getX / pxPerSec() - snapOffset, startPosition, true) + snapOffset - dragOffset
      val fitting = selectionActive((dragWidth + x) * pxPerSec()) && x >= 0 && list.forall(elem => clazz.isInstance(elem._3) && content.intersecting(elem._1 + x, elem._2 + x).isEmpty)
      if (fitting) {
        val insert = list.map(elem => (elem._1 + x, elem._2 + x, elem._3.toMutable(Some(undoManager)).asInstanceOf[A]))
        content ++= insert
        selectedElements.clear()
        for (elem <- insert) {
          selectedElements += getView((elem._1, elem._3))
        }
        e.setDropCompleted(true)
      }
      e.consume()
    }
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