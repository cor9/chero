package io.gitlab.sklavedaniel.beatmetergenerator

import java.io._
import java.util
import javafx.collections.{FXCollections, ObservableList}

import scala.collection.JavaConverters.asJavaCollection
import scala.collection.mutable
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
import scalafx.scene.input.MouseButton
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
    val input = opt[File](required = true, descr = "Audio or video file to play")
    val beats = opt[File](descr = "File containing beat definitions")
    validateFileExists(input)
    validateFileExists(beats)

    def execute(args: Array[String]): Unit = {
      new BeatEditor(this).main(args)
    }
  }

}

class BeatEditor(conf: BeatEditor.Conf) extends JFXApp {

  val player = new AudioPlayer(new BufferedInputStream(new FileInputStream(conf.input())), new BufferedInputStream(getClass.getResourceAsStream("/beats/click.wav")))
  var playerRunning = false
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

  def insertBeat(beat: Double): Unit = {
    if (beats.isEmpty) {
      beats.insert(0, beat)
    } else {
      insertBeat(beat, 0, beats.size - 1)
    }
  }

  def insertBeat(beat: Double, first: Int, last: Int): Unit = {
    if (first == last) {
      if (beats(first) < beat) {
        beats.add(beat)
      } else if (beat < beats(first)) {
        beats.insert(first, beat)
      }
    } else {
      val pos = (first + last) / 2
      if (beats(pos) < beat) {
        insertBeat(beat, pos + 1, last)
      } else if (beat < beats(pos)) {
        insertBeat(beat, first, pos)
      }
    }
  }

  val beatsView = new ListView(beats) {
    hgrow = Priority.Always
    minHeight = 200
    editable = true
    cellFactory = TextFieldListCell.forListView(new DoubleStringConverter())
    onEditCommit = (t: ListView.EditEvent[Double]) => {
      beats.remove(t.index)
      insertBeat(t.getNewValue)
    }
  }
  beatsView.getSelectionModel.setSelectionMode(SelectionMode.Multiple)
  val rateSlider = new Slider {
    min = 0.1
    max = 1.0
    value = 0.5
    blockIncrement = 0.1
  }
  rateSlider.value.onChange { (_, _, d) =>
    if (playerRunning && !playerSuspended) player.pause()
    player.rate.set(d.floatValue())
    if (playerRunning && !playerSuspended) {
      player.beats.set(beats.toList)
      player.play()
    }
  }
  rateSlider.onMousePressed() = handle {
    if (playerRunning) {
      player.pause()
      playerSuspended = true
    }
  }
  rateSlider.onMouseReleased() = handle {
    if (playerRunning) {
      playerSuspended = false
      player.beats.set(beats.toList)
      player.play()
    }
  }
  val ratioSlider = new Slider {
    min = 0.0
    max = 1.0
    value = 0.8
    blockIncrement = 0.1
  }
  ratioSlider.value.onChange { (_, _, d) =>
    if (playerRunning && !playerSuspended) player.pause()
    player.ratio.set(d.floatValue())
    if (playerRunning && !playerSuspended) {
      player.beats.set(beats.toList)
      player.play()
    }
  }
  ratioSlider.onMousePressed() = handle {
    if (playerRunning) {
      player.pause()
      playerSuspended = true
    }
  }
  ratioSlider.onMouseReleased() = handle {
    if (playerRunning) {
      playerSuspended = false
      player.beats.set(beats.toList)
      player.play()
    }
  }

  val positionSlider = new Slider {
    min = 0
    max = player.duration
    value = 0
    blockIncrement = 1
  }
  val secondWidth = 100
  val volumeHeight = 150
  positionSlider.value.onChange { (a, _, d) =>
    if (playerRunning && !playerSuspended && !playerIgnore) player.pause()
    player.position.set(d.doubleValue())
    position.text() = f"${d.doubleValue()}%07.2f s"
    positionLine.startX = 100 + d.doubleValue() * secondWidth
    positionLine.endX = 100 + d.doubleValue() * secondWidth
    waveView.layoutX = -(d.doubleValue() * secondWidth)
    if (playerRunning && !playerSuspended && !playerIgnore) {
      player.beats.set(beats.toList)
      player.play()
    }
  }
  positionSlider.onMousePressed() = handle {
    if (playerRunning) {
      player.pause()
      playerSuspended = true
    }
  }
  positionSlider.onMouseReleased() = handle {
    if (playerRunning) {
      playerSuspended = false
      player.beats.set(beats.toList)
      player.play()
    }
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

  stage = new PrimaryStage {
    self =>
    scene = new Scene {
      root = new VBox {
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
                      new Button {
                        text = "Play"
                        onAction = handle {
                          playerRunning = true
                          player.beats.set(beats.toList)
                          player.play()
                        }
                      },
                      new Button {
                        text = "Beat"
                        armed.onChange((_, _, x) => {
                          if (x) insertBeat(player.currentPosition)
                        })
                      },
                      new Button {
                        text = "Pause"
                        onAction = handle {
                          player.pause()
                          playerRunning = false
                        }

                      }
                    )
                  },
                  new HBox {
                    spacing = 5
                    val positionField = new TextField {
                      prefWidth = 50
                    }

                    children = Seq(
                      new Button {
                        text = "Remove"
                        onAction = handle {
                          for (i <- beatsView.getSelectionModel.getSelectedIndices.reverse) {
                            beats.remove(i, i + 1)
                          }
                        }
                      },
                      new Button {
                        text = "Add"
                        onAction = handle {
                          insertBeat(positionField.getText.toDouble)
                        }
                      },
                      positionField,
                      new Button {
                        text = "Align equally"
                        onAction = handle {
                          val bs = beatsView.getSelectionModel.getSelectedItems.toList
                          if (beatsView.getSelectionModel.getSelectedIndices.size >= 3) {
                            val distance = (bs.last - bs.head) / (bs.length - 1)
                            beatsView.getSelectionModel.clearSelection()
                            beats.removeAll(bs: _*)
                            for (i <- bs.indices) {
                              val b = i * distance + bs.head
                              insertBeat(b)
                              beatsView.getSelectionModel.select(b)
                            }
                          }
                        }
                      }
                    )
                  },
                  new HBox {
                    spacing = 5
                    val stepSpinner = new Spinner[Double](0.01, 60, 0.05, 0.05) {
                      prefWidth = 80
                      editable = true
                    }

                    def moveSelected(delta: Double): Unit = {
                      val bs = beatsView.getSelectionModel.getSelectedItems.toList
                      beatsView.getSelectionModel.clearSelection()
                      beats.removeAll(bs: _*)
                      for (b <- bs) {
                        insertBeat(b + delta)
                        beatsView.getSelectionModel.select(b + delta)
                      }
                    }

                    children = Seq(
                      new Button {
                        text = "-"
                        onAction = handle {
                          moveSelected(-stepSpinner.value())
                        }
                      },
                      stepSpinner,
                      new Button {
                        text = "+"
                        onAction = handle {
                          moveSelected(stepSpinner.value())
                        }
                      },
                      new Button {
                        text = "Left to now"
                        onAction = handle {
                          if (!beatsView.getSelectionModel.getSelectedIndices.isEmpty) {
                            val offset = beatsView.getSelectionModel.getSelectedItems.head
                            moveSelected(-offset + positionSlider.value())
                          }
                        }
                      },
                      new Button {
                        text = "Right to now"
                        onAction = handle {
                          if (!beatsView.getSelectionModel.getSelectedIndices.isEmpty) {
                            val offset = beatsView.getSelectionModel.getSelectedItems.last
                            moveSelected(-offset + positionSlider.value())
                          }
                        }
                      }
                    )
                  },
                  new HBox {
                    spacing = 5
                    val repetitionSpinner = new Spinner[Int](1, Int.MaxValue, 1) {
                      prefWidth = 80
                      editable = true
                    }
                    children = Seq(
                      new Button {
                        text = "Copy"
                        onAction = handle {
                          copiedBeats = beatsView.getSelectionModel.getSelectedItems.toList
                        }
                      },
                      new Button {
                        text = "Insert Left"
                        onAction = handle {
                          if (!copiedBeats.isEmpty)
                            for (_ <- 1 to repetitionSpinner.value()) {
                              beatsView.getSelectionModel.clearSelection()
                              for (b <- copiedBeats) {
                                val tmp = b - copiedBeats.head + positionSlider.value()
                                insertBeat(tmp)
                                beatsView.getSelectionModel.select(tmp)
                              }
                              positionSlider.value() = copiedBeats.last - copiedBeats.head + positionSlider.value()
                            }
                        }
                      },
                      new Button {
                        text = "Insert Right"
                        onAction = handle {
                          if (!copiedBeats.isEmpty)
                            for (_ <- 1 to repetitionSpinner.value()) {
                              beatsView.getSelectionModel.clearSelection()
                              for (b <- copiedBeats) {
                                val tmp = b - copiedBeats.last + positionSlider.value()
                                insertBeat(tmp)
                                beatsView.getSelectionModel.select(tmp)
                              }
                              positionSlider.value() = copiedBeats.head - copiedBeats.last + positionSlider.value()
                            }
                        }
                      },
                      repetitionSpinner
                    )
                  },
                  new HBox {
                    spacing = 5
                    alignment = Pos.BaselineLeft
                    val repetitionSpinner = new Spinner[Int](1, Int.MaxValue, 2) {
                      prefWidth = 80
                      editable = true
                    }
                    val restSpinner = new Spinner[Int](0, Int.MaxValue, 0) {
                      prefWidth = 80
                      editable = true
                    }
                    children = Seq(
                      new Button {
                        text = "Align Pattern"
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
                            beats.removeAll(bs: _*)
                            for (i <- bs.indices) {
                              val b = (i / patternSize) * patternDuration + avgs(i % patternSize) + bs.head
                              insertBeat(b)
                              beatsView.getSelectionModel.select(b)
                            }
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
                  new HBox {
                    spacing = 5
                    children = Seq(
                      new Button {
                        text = "Load"
                        onAction = handle {

                          import scala.collection.JavaConverters._

                          val fileChooser = new FileChooser()
                          fileChooser.setTitle("Open Beats File")
                          val file = fileChooser.showOpenDialog(stage)
                          if (file != null)
                            beats.setAll(asJavaCollection(BeatFiles.load(file)))
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

