package io.gitlab.sklavedaniel.beatmetergenerator

import java.io._
import java.util
import javafx.collections.FXCollections

import scalafx.Includes._
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.scene.control._
import scalafx.scene.control.cell.TextFieldListCell
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Line}
import scalafx.scene.text.Text
import scalafx.scene.{Group, Scene}
import scalafx.stage.FileChooser
import scalafx.util.converter.DoubleStringConverter

object BeatEditor {

  class Conf extends Main.ExecutableSubcommand("editor") {
    val input = opt[File](required = true, descr = "Audio or video file to play")

    def execute(args: Array[String]): Unit = {
      new BeatEditor(this).main(args)
    }
  }

}

class BeatEditor(conf: BeatEditor.Conf) extends JFXApp {

  val player = new AudioPlayer(new BufferedInputStream(new FileInputStream(conf.input())), getClass.getResourceAsStream("/beats/click.wav"))

  var restartPlayer = false
  player.ratio = 0.8
  player.rate = 0.5f
  player.listener = Some((d, s) => {
    Platform.runLater {
      restartPlayer = false
      positionSlider.value() = d
      if (s != AudioPlayer.Stopped)
        restartPlayer = true
    }
  })

  override def stopApp(): Unit = {
    player.pause()
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
  val beats = FXCollections.observableList(new util.ArrayList[Double]())

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
  }
  rateSlider.value.onChange { (_, _, d) =>
    val b = restartPlayer
    if (b) player.pause()
    player.rate = d.floatValue()
    if (b) player.play()
  }
  val ratioSlider = new Slider {
    min = 0.0
    max = 1.0
    value = 0.8
  }
  ratioSlider.value.onChange { (_, _, d) =>
    val b = restartPlayer
    if (b) player.pause()
    player.ratio = d.floatValue()
    if (b) player.play()
  }

  val positionSlider = new Slider {
    min = 0
    max = player.duration
    value = 0
  }
  val secondWidth = 100
  val volumeHeight = 150
  positionSlider.value.onChange { (_, _, d) =>
    val b = restartPlayer
    if (b) player.pause()
    player.position = d.doubleValue()
    position.text() = f"${d.doubleValue()}%07.2f s"
    positionLine.startX = 100 + d.doubleValue() * secondWidth
    positionLine.endX = 100 + d.doubleValue() * secondWidth
    waveView.layoutX = -(d.doubleValue() * secondWidth)
    if (b) player.play()
  }

  val waveGroup = new Group {
    layoutX = 0
    layoutY = 0
    children = (for (Seq((avg1, time1), (avg2, time2)) <- player.avgs.sliding(2)) yield {new Line {
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
  }
  class Beat(val time: Double) extends Circle {
      radius = 4
      centerY = 170
      centerX = 100 + time * secondWidth
      fill = Color.Red
      onMouseClicked = handle {
        beatsView.selectionModel().select(time)
        beatsView.scrollTo(time)
      }
  }
  beats.onChange {
    beatsGroup.children.clear()
    for (beat <- beats) {
      beatsGroup.children.add(new Beat(beat))
    }
  }

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
          beatsView,
          new HBox {
            spacing = 5
            children = List(
              new Button {
                text = "Play"
                onAction = handle {
                  player.beats = beats.toSeq
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
                }

              },
              new Button {
                text = "Jump"
                onAction = handle {
                  val idx = beatsView.getSelectionModel.getSelectedIndex
                  if (idx != -1) {
                    positionSlider.value() = beats(idx)
                  }

                }
              },
              new Button {
                text = "Remove"
                onAction = handle {
                  for (i <- beatsView.getSelectionModel.getSelectedIndices.reverse) {
                    beats.remove(i, i + 1)
                  }
                }
              }
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
    }
  }

}

