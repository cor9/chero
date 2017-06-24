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

object BeatEditor {

  class Conf extends Main.ExecutableSubcommand("editor") {
    val input = opt[File](required = true, descr = "Audio file to play")
    val beats = opt[File](descr = "File containing beat definitions")
    validateFileExists(input)
    validateFileExists(beats)

    def execute(subcommands: List[ScallopConf], args: Array[String]): Unit = {
      new BeatEditor(this).main(args)
    }
  }

}

class BeatEditor(conf: BeatEditor.Conf) extends JFXApp {

  val player = new AudioPlayer(new BufferedInputStream(new FileInputStream(conf.input())), new BufferedInputStream(getClass.getResourceAsStream("/beats/click.wav")))
  var playerSuspended = false
  var playerIgnore = false

  player.ratio.set(0.8)
  player.rate.set(0.5f)
  player.listener.set(Some((d, s) => {
    Platform.runLater {
      playerIgnore = true
      positionSlider.value() = d
      playerIgnore = false
    }
  }))

  override def stopApp(): Unit = {
    player.terminate()
  }

  val duration = new Text {
    text = f"${
      player.duration
    }%07.2f s"
  }
  val position = new Text {
    text = f"${
      0.0
    }%07.2f s"
  }
  val beats: ObservableList[Double] = FXCollections.observableList(new util.ArrayList[Double](
    asJavaCollection(conf.beats.toOption.map(BeatFiles.load(_)).getOrElse(Seq[Double]()))
  ))
  var beatsUndo: List[(Set[Double], Set[Double])] = Nil
  var beatsRedo: List[(Set[Double], Set[Double])] = Nil

  def changeBeats(insert: Seq[Double] = Nil, remove: Seq[Double] = Nil, removeIdx: Seq[Int] = Nil, removeIdxInteger: Seq[Integer] = Nil): Unit = {
    val removed = (for (b <- removeIdx.reverse) yield {
      beats.remove(b)
    }) ++ (for (b <- removeIdxInteger.reverse) yield {
      beats.remove(b.toInt)
    }) ++ (for {
      b <- remove
      if beats.remove(b)
    } yield b)
    val inserted = for {
      b <- insert
      if insertBeat(b)
    } yield b
    beatsUndo = (removed.toSet, inserted.toSet) :: beatsUndo
    beatsRedo = Nil
  }

  def undo(): Unit = {
    beatsUndo match {
      case (insert, remove) :: tail =>
        beatsUndo = tail
        beatsRedo = (remove, insert) :: beatsRedo
        for (b <- remove)
          beats.remove(b)
        for (b <- insert)
          insertBeat(b)
      case Nil =>
    }
  }

  def redo(): Unit = {
    beatsRedo match {
      case (insert, remove) :: tail =>
        beatsRedo = tail
        beatsUndo = (remove, insert) :: beatsUndo
        for (b <- remove)
          beats.remove(b)
        for (b <- insert)
          insertBeat(b)
      case Nil =>
    }
  }

  def insertBeat(beat: Double): Boolean = {
    if (beats.isEmpty) {
      beats.insert(0, beat)
      true
    } else {
      insertBeat(beat, 0, beats.size - 1)
    }
  }

  def insertBeat(beat: Double, first: Int, last: Int): Boolean = {
    if (first == last) {
      if (beats(first) < beat) {
        beats.add(beat)
        true
      } else if (beat < beats(first)) {
        beats.insert(first, beat)
        true
      } else {
        false
      }
    } else {
      val pos = (first + last) / 2
      if (beats(pos) < beat) {
        insertBeat(beat, pos + 1, last)
      } else if (beat < beats(pos)) {
        insertBeat(beat, first, pos)
      } else {
        false
      }
    }
  }

  val beatsView = new ListView(beats) {
    hgrow = Priority.Always
    minHeight = 200
    editable = true
    cellFactory = TextFieldListCell.forListView(new DoubleStringConverter())
    onEditCommit = (t: ListView.EditEvent[Double]) => {
      changeBeats(removeIdx = Seq(t.index), insert = Seq(t.getNewValue))
    }
  }
  beatsView.getSelectionModel.setSelectionMode(SelectionMode.Multiple)

  def suspendPlayer(): Unit = {
    if (playButton.selected()) {
      player.pause()
      playerSuspended = true
    }
  }

  def unsuspendPlayer(): Unit = {
    if (playButton.selected()) {
      playerSuspended = false
      player.beats.set(beats.toList)
      player.play()
    }
  }

  val rateSlider = new Slider {
    tooltip = new Tooltip("Playback rate")
    min = 0.1
    max = 1.0
    value = 0.5
    blockIncrement = 0.1
    value.onChange { (_, _, d) =>
      if (playButton.selected() && !playerSuspended) player.pause()
      player.rate.set(d.floatValue())
      if (playButton.selected() && !playerSuspended) {
        player.beats.set(beats.toList)
        player.play()
      }
    }
    onMousePressed() = handle(suspendPlayer())
    onMouseReleased() = handle(unsuspendPlayer())
  }

  val ratioSlider = new Slider {
    tooltip = new Tooltip("Volume ratio of beats")
    min = 0.0
    max = 1.0
    value = 0.8
    blockIncrement = 0.1
    value.onChange { (_, _, d) =>
      if (playButton.selected() && !playerSuspended) player.pause()
      player.ratio.set(d.floatValue())
      if (playButton.selected() && !playerSuspended) {
        player.beats.set(beats.toList)
        player.play()
      }
    }
    onMousePressed() = handle(suspendPlayer())
    onMouseReleased() = handle(unsuspendPlayer())
  }


  val secondWidth = 100
  val volumeHeight = 150
  val positionSlider: Slider = new Slider {
    min = 0
    max = player.duration
    value = 0
    blockIncrement = 1

    value.onChange { (a, _, d) =>
      if (playButton.selected() && !playerSuspended && !playerIgnore) player.pause()
      player.position.set(d.doubleValue())
      position.text() = f"${d.doubleValue()}%07.2f s"
      positionLine.startX = 100 + d.doubleValue() * secondWidth
      positionLine.endX = 100 + d.doubleValue() * secondWidth
      waveView.layoutX = -(d.doubleValue() * secondWidth)
      if (playButton.selected() && !playerSuspended && !playerIgnore) {
        player.beats.set(beats.toList)
        player.play()
      }
    }
    onMousePressed() = handle(suspendPlayer())
    onMouseReleased() = handle(unsuspendPlayer())
  }

  val waveGroup = new Group {
    layoutX = 0
    layoutY = 0
    children = (for (Seq((avg1, time1), (avg2, time2)) <- player.avgs.sliding(2)) yield {
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
            val index = beats.indexOf(time)
            val selected = beatsView.selectionModel().isSelected(index)
            val selectedIndex = beatsView.selectionModel().getSelectedIndex
            if (!e.isControlDown) {
              beatsView.selectionModel().clearSelection()
            }
            if (e.isShiftDown) {
              if (selectedIndex != -1) {
                if (index < selectedIndex) {
                  beatsView.selectionModel().selectRange(index, selectedIndex + 1)
                } else if (selectedIndex < index) {
                  beatsView.selectionModel().selectRange(selectedIndex, index + 1)
                }
              }
            } else if (selected) {
              beatsView.selectionModel().clearSelection(index)
            } else {
              beatsView.selectionModel().select(index)
            }
            beatsView.scrollTo(index)
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
  beats.onChange { (_, changes) =>
    for (change <- changes) {
      change match {
        case ObservableBuffer.Add(index, t) =>
          for (beat <- t) {
            val b = new Beat(beat, beatsView.selectionModel().isSelected(index))
            beatIcons.put(beat, b)
            beatsGroup.children.add(b)
          }
        case ObservableBuffer.Remove(_, t) =>
          for (beat <- t)
            beatsGroup.children.remove(beatIcons.remove(beat).get)
        case c => assert(false, c.toString)
      }
    }
  }
  beatsView.selectionModel().selectedItems.onChange { (_, changes) =>
    for (change <- changes) {
      change match {
        case ObservableBuffer.Add(_, t) =>
          for (beat <- t) {
            beatIcons(beat).selected() = true
          }
        case ObservableBuffer.Remove(_, t) =>
          for (beat <- t) {
            beatIcons(beat).selected() = false
          }
        case c => assert(false, c.toString)
      }
    }
  }

  var copiedBeats: Seq[Double] = Seq()

  val playButton = new ToggleButton {
    text = "Play"
    selected.onChange { (_, _, b) =>
      if (b) {
        player.beats.set(beats.toList)
        player.play()
      } else {
        player.pause()
      }
    }
  }

  stage = new PrimaryStage {
    self =>
    scene = new Scene {
      val beatButton = new Button {
        text = "Beat"
        tooltip = new Tooltip("Insert beat at current position")
        armed.onChange((_, _, x) => {
          if (x) changeBeats(insert = Seq(player.currentPosition))
        })
      }
      val removeButton = new Button {
        text = "Remove"
        tooltip = new Tooltip("Remove selected beats")
        onAction = handle {
          changeBeats(removeIdxInteger = beatsView.selectionModel().getSelectedIndices.toList)
        }
      }
      val positionField = new Spinner(0.0, player.duration, 0.0, 1.0) {
        tooltip = new Tooltip("Position of beat to add")
        editable = true
        prefWidth = 80
      }
      val addButton = new Button {
        text = "Add"
        tooltip = new Tooltip("Add beat at set position")
        onAction = handle {
          changeBeats(insert = Seq(positionField.value()))
        }
      }
      val alignEquallyButton = new Button {
        text = "Align equally"
        tooltip = new Tooltip("Allign selected beats with equal distance")
        onAction = handle {
          val bs = beatsView.getSelectionModel.getSelectedItems.toList
          if (beatsView.getSelectionModel.getSelectedIndices.size >= 3) {
            val distance = (bs.last - bs.head) / (bs.length - 1)
            beatsView.getSelectionModel.clearSelection()
            val nbs = (for (i <- bs.indices) yield i * distance + bs.head).toList
            changeBeats(remove = bs, insert = nbs)
            for (b <- nbs)
              beatsView.getSelectionModel.select(b)
          }
        }
      }
      val stepSpinner = new Spinner[Double](0.01, 60, 0.05, 0.05) {
        tooltip = new Tooltip("Distance to move selected beats by")
        prefWidth = 80
        editable = true
      }

      def moveSelected(delta: Double): Unit = {
        val bs = beatsView.getSelectionModel.getSelectedItems.toList
        beatsView.getSelectionModel.clearSelection()
        val nbs = for (b <- bs) yield b + delta
        changeBeats(remove = bs, insert = nbs)
        for (b <- nbs)
          beatsView.getSelectionModel.select(b)
      }

      val moveLeftButton = new Button {
        text = "-"
        tooltip = new Tooltip("Move selected beats to the left by set distance")
        onAction = handle {
          moveSelected(-stepSpinner.value())
        }
      }
      val moveRightButton = new Button {
        text = "+"
        tooltip = new Tooltip("Move selected beats to the right by set distance")
        onAction = handle {
          moveSelected(stepSpinner.value())
        }
      }
      val rightToNowButton = new Button {
        text = "Right to now"
        tooltip = new Tooltip("Move selected beats left of current position")
        onAction = handle {
          if (!beatsView.getSelectionModel.getSelectedIndices.isEmpty) {
            val offset = beatsView.getSelectionModel.getSelectedItems.head
            moveSelected(-offset + positionSlider.value())
          }
        }
      }
      val leftToNowButton = new Button {
        text = "Left to now"
        tooltip = new Tooltip("Move selected beats right of current position")
        onAction = handle {
          if (!beatsView.getSelectionModel.getSelectedIndices.isEmpty) {
            val offset = beatsView.getSelectionModel.getSelectedItems.last
            moveSelected(-offset + positionSlider.value())
          }
        }
      }
      val repetitionSpinner = new Spinner[Int](1, Int.MaxValue, 1) {
        tooltip = new Tooltip("Repetitons of copied beats")
        prefWidth = 80
        editable = true
      }
      val overwriteCheckbox = new CheckBox("overwrite") {
        tooltip = new Tooltip("When inserting remove existing beats")
        selected = true
      }
      val snapCheckbox = new CheckBox("snap") {
        tooltip = new Tooltip("When inserting snap to existing beats")
        selected = true
      }
      val copyButton = new Button {
        text = "Copy"
        tooltip = new Tooltip("Copy selected beats")
        onAction = handle {
          copiedBeats = beatsView.getSelectionModel.getSelectedItems.toList
        }
      }
      val snapDelta = 0.02

      def computeBeats(nbs: Seq[Double]) = {
        val sbs = beats.dropWhile(_ <= nbs.head - snapDelta).takeWhile(_ <= nbs.last + snapDelta)
        val snbs = if (snapCheckbox.selected()) {
          nbs.foldLeft((0.0, List[Double]())) { case ((shift, result), b) =>
            val beat = b + shift
            val min = sbs.minBy(b => (b - beat).abs)
            var delta = min - beat
            if (delta.abs <= snapDelta) {
              (shift + delta, (beat + delta) :: result)
            } else {
              (shift, beat :: result)
            }
          }._2.reverse
        } else nbs
        val rbs = if (overwriteCheckbox.selected()) {
          beats.dropWhile(_ <= snbs.head).takeWhile(_ <= snbs.last)
        } else Nil
        changeBeats(remove = rbs, insert = snbs)
        for (b <- snbs)
          beatsView.getSelectionModel.select(b)
        snbs
      }

      val insertRightButton = new Button {
        text = "Insert Right"
        tooltip = new Tooltip("Insert copied beats, right of current position")
        onAction = handle {
          if (copiedBeats.nonEmpty)
            for (_ <- 1 to repetitionSpinner.value()) {
              beatsView.getSelectionModel.clearSelection()
              val nbs = (for (b <- copiedBeats) yield b - copiedBeats.head + positionSlider.value()).toList
              val snbs = computeBeats(nbs)
              positionSlider.value() = snbs.last
            }
        }
      }
      val insertLeftButton = new Button {
        text = "Insert Left"
        tooltip = new Tooltip("Insert copied beats, left of current position")
        onAction = handle {
          if (copiedBeats.nonEmpty)
            for (_ <- 1 to repetitionSpinner.value()) {
              beatsView.getSelectionModel.clearSelection()
              val nbs = (for (b <- copiedBeats) yield b - copiedBeats.last + positionSlider.value())
              val snbs = computeBeats(nbs)
              positionSlider.value() = snbs.head
            }
        }
      }
      val shortcuts = Seq[(KeyCombination, () => Unit)](
        KeyCombination("p") -> (() => playButton.fire()),
        KeyCombination("b") -> (() => {
          beatButton.arm()
          beatButton.fire()
          beatButton.disarm()
        }),
        KeyCombination("ctrl+left") -> (() => moveLeftButton.fire()),
        KeyCombination("ctrl+right") -> (() => moveRightButton.fire()),
        KeyCombination("left") -> (() => positionSlider.value() -= 1),
        KeyCombination("right") -> (() => positionSlider.value() += 1),
        KeyCombination("alt+left") -> (() => positionSlider.value() -= 0.05),
        KeyCombination("alt+right") -> (() => positionSlider.value() += 0.05),
        KeyCombination("shift+left") -> (() => positionSlider.value() -= 10),
        KeyCombination("shift+right") -> (() => positionSlider.value() += 10),
        KeyCombination("n") -> (() => rightToNowButton.fire()),
        KeyCombination("shift+n") -> (() => leftToNowButton.fire()),
        KeyCombination("c") -> (() => copyButton.fire()),
        KeyCombination("v") -> (() => insertRightButton.fire()),
        KeyCombination("shift+v") -> (() => insertLeftButton.fire()),
        KeyCombination("e") -> (() => alignEquallyButton.fire()),
        KeyCombination("d") -> (() => beatsView.selectionModel().clearSelection()),
        KeyCombination("delete") -> (() => removeButton.fire()),
        KeyCombination("z") -> (() => undo()),
        KeyCombination("shift+z") -> (() => redo())
      )
      val digitKeyCodes = (Seq(KeyCode.Digit1, KeyCode.Digit2, KeyCode.Digit3, KeyCode.Digit4, KeyCode.Digit5,
        KeyCode.Digit6, KeyCode.Digit7, KeyCode.Digit8, KeyCode.Digit9, KeyCode.Digit0).zipWithIndex).toMap

      addEventFilter(KeyEvent.KeyPressed, (e: input.KeyEvent) => {
        shortcuts.find(_._1.`match`(e)) match {
          case Some((_, f)) =>
            f()
            e.consume()
          case None =>
            if (e.isControlDown) {
              digitKeyCodes.get(e.getCode).foreach { i =>
                val j = beats.indexWhere(_ >= positionSlider.value())
                if (beats.size - j >= i + 1) {
                  if (e.isAltDown) {
                    positionSlider.value() = beats(j + i)
                  } else {
                    if (beatsView.selectionModel().isSelected(j + i)) {
                      beatsView.selectionModel().clearSelection(j + i)
                    } else {
                      beatsView.selectionModel().select(j + i)
                    }
                  }
                }
                e.consume()
              }
            }
        }
      })
      root = new VBox {
        prefWidth = 800
        spacing = 5
        children = Seq(
          new Pane {
            minHeight = 200
            children = Seq(waveView)
          },
          positionSlider,
          new HBox {
            spacing = 5
            hgrow = Priority.Always
            children = Seq(
              beatsView,
              new VBox {
                spacing = 5
                children = Seq(
                  new HBox {
                    spacing = 5
                    children = Seq(
                      playButton,
                      beatButton,
                      new Button {
                        text = "undo"
                        onAction = handle(undo())
                      },
                      new Button {
                        text = "redo"
                        onAction = handle(redo())
                      }
                    )
                  },
                  new Separator(),
                  new HBox {
                    spacing = 5
                    children = Seq(
                      removeButton,
                      addButton,
                      positionField,
                      alignEquallyButton
                    )
                  },
                  new HBox {
                    spacing = 5
                    alignment = Pos.BaselineLeft
                    val bpmSpinner = new Spinner[Double](10.0, 600.0, 120.0, 1.0) {
                      tooltip = new Tooltip("Bpm to generate")
                      editable = true
                      prefWidth = 80
                    }
                    val durationSpinner = new Spinner[Double](0.0, player.duration, 60.0, 10.0) {
                      tooltip = new Tooltip("Duration to generate for")
                      prefWidth = 80
                    }
                    children = Seq(
                      new Button {
                        text = "Add bpm"
                        tooltip = new Tooltip("Add beats at set bpm from current position")
                        onAction = handle {
                          val count = (durationSpinner.value() * bpmSpinner.value() / 60.0).toInt
                          val nbs = for (i <- 0 until count) yield positionSlider.value() + i * 60.0 / bpmSpinner.value()
                          changeBeats(insert = nbs)
                        }
                      },
                      bpmSpinner,
                      new Text(" for "),
                      durationSpinner

                    )
                  },
                  new HBox {
                    spacing = 5
                    children = Seq(
                      moveLeftButton,
                      stepSpinner,
                      moveRightButton,
                      leftToNowButton,
                      rightToNowButton
                    )
                  },
                  new Separator(),
                  new HBox {
                    spacing = 5
                    children = Seq(
                      copyButton,
                      insertLeftButton,
                      insertRightButton,
                      repetitionSpinner,
                      snapCheckbox,
                      overwriteCheckbox
                    )
                  },
                  new HBox {
                    spacing = 5
                    alignment = Pos.BaselineLeft
                    children = Seq(
                      new Text("repeat"),
                      repetitionSpinner,
                      snapCheckbox,
                      overwriteCheckbox
                    )
                  },
                  new Separator(),
                  new HBox {
                    spacing = 5
                    alignment = Pos.BaselineLeft
                    val repetitionSpinner = new Spinner[Int](1, Int.MaxValue, 2) {
                      prefWidth = 80
                      editable = true
                      tooltip = new Tooltip("Complete repetitions including an extra beat marking the end")
                    }
                    val restSpinner = new Spinner[Int](0, Int.MaxValue, 0) {
                      prefWidth = 80
                      editable = true
                      tooltip = new Tooltip("Number of beats in incomplete pattern")
                    }
                    children = Seq(
                      new Button {
                        text = "Align Pattern"
                        tooltip = new Tooltip("Align repeating pattern of beats")
                        onAction = handle {
                          val bs = beatsView.getSelectionModel.getSelectedItems.toVector
                          if (repetitionSpinner.value() + restSpinner.value() >= 2
                            && beatsView.getSelectionModel.getSelectedIndices.size >= 2
                            && (bs.size - restSpinner.value() - 1) % repetitionSpinner.value() == 0
                          ) {
                            val patternSize = (bs.size - restSpinner.value() - 1) / repetitionSpinner.value()
                            val alignCount = (bs.size - 1) / patternSize
                            val alignPos = alignCount * patternSize
                            val alignDuration = bs(alignPos) - bs.head
                            val patternDuration = alignDuration / alignCount
                            val avgs: IndexedSeq[Double] = for (i <- 0 to patternSize) yield {
                              val tmp: IndexedSeq[Double] = for {
                                j <- 0 to bs.size / patternSize
                                pos = j * patternSize + i
                                if pos < bs.size
                              } yield bs(pos) - patternDuration * j - bs.head
                              tmp.sum / tmp.size
                            }
                            beatsView.getSelectionModel.clearSelection()
                            val nbs = (for (i <- bs.indices) yield
                              (i / patternSize) * patternDuration + avgs(i % patternSize) + bs.head).toList
                            changeBeats(remove = bs, insert = nbs)
                            for (b <- nbs)
                              beatsView.getSelectionModel.select(b)
                          }
                        }
                      },
                      new Text {
                        text = "Rep."
                      },
                      repetitionSpinner,
                      new Text {
                        text = "Rest"
                      },
                      restSpinner
                    )
                  },
                  new Separator(),
                  new HBox {
                    spacing = 5
                    children = Seq(new Text {
                      text = "Speed"
                    }, rateSlider, new Text {
                      text = "Beats"
                    }, ratioSlider)
                  },
                  new HBox {
                    spacing = 5
                    children = Seq(
                      new Text {
                        text = "Current:"
                      }, position,
                      new Text {
                        text = "Duration:"
                      }, duration
                    )
                  },
                  new Separator(),
                  new HBox {
                    spacing = 5
                    children = Seq(
                      new Button {
                        text = "Load"
                        onAction = handle {
                          val fileChooser = new FileChooser()
                          fileChooser.setTitle("Open Beats File")
                          val file = fileChooser.showOpenDialog(stage)
                          if (file != null)
                            changeBeats(remove = beats.toList, insert = BeatFiles.load(file))
                        }
                      },
                      new Button {
                        text = "Store"
                        onAction = handle {
                          val fileChooser = new FileChooser()
                          fileChooser.setTitle("Save Beats File")
                          val file = fileChooser.showSaveDialog(stage)
                          if (file != null)
                            BeatFiles.store(file, beats)
                        }
                      }
                    )
                  }
                )
              }
            )
          }
        )
      }
    }
  }

}

