package io.gitlab.sklavedaniel.beatmetergenerator

import java.io._
import java.util
import javafx.collections.FXCollections

import org.rogach.scallop.Subcommand

import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.scene.Scene
import scalafx.scene.control.SpinnerValueFactory.DoubleSpinnerValueFactory
import scalafx.scene.control._
import scalafx.scene.control.cell.TextFieldListCell
import scalafx.scene.layout._
import scalafx.scene.media.{Media, MediaPlayer}
import scalafx.scene.text.Text
import scalafx.stage.FileChooser
import scalafx.util.Duration
import scalafx.util.converter.DoubleStringConverter

object BeatEditor {
  class Conf extends Subcommand("editor") {
    val input = opt[File](required = true, descr = "Audio or video file to play")
  }
}
class BeatEditor(conf: BeatEditor.Conf) extends JFXApp {

  val media = new Media(conf.input().toURI.toURL.toString)
  val player = new MediaPlayer(media)

  val beatMedia = new Media(getClass.getResource("/beats/click.wav").toURI.toString)
  val beatMediaPlayer = new MediaPlayer(beatMedia)

  player.setVolume(0.1)
  player.setRate(0.5)
  player.onPlaying() = () => {
    beatTimeline.foreach(_.playFrom(player.getCurrentTime))
  }

  val valueFactory = new DoubleSpinnerValueFactory(0, 0, 0)
  valueFactory.amountToStepBy = 1000
  val duration = new Text {
    text = f"${0.0}%07.1f ms"
  }
  val position = new Text {
    text = f"${0.0}%07.1f ms"
  }
  player.currentTime.onChange((_, _, y) => {
    if (!y.isUnknown)
      Platform.runLater(position.text() = f"${y.toMillis}%07.1f ms")
  })
  player.totalDuration.onChange((_, _, y) => {
    if (!y.isUnknown) {
      Platform.runLater(valueFactory.max = y.toMillis)
      duration.text = f"${y.toMillis}%07.1f ms"
    }
  })
  valueFactory.value.onChange((_, _, x) => {
    player.seek(Duration(x))
  })
  val beats = FXCollections.observableList(new util.ArrayList[Double]())
  var beatTimeline: Option[Timeline] = None

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

  val durationSpinner = new Spinner(valueFactory) {
    editable = true
    prefWidth = 100
  }

  val beatsView = new ListView(beats) {
    minHeight = 300
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
  stage = new PrimaryStage {
    self =>
    scene = new Scene {
      root = new VBox {
        spacing = 5
        children = Seq(
          beatsView,
          new HBox {
            spacing = 5
            children = List(
              new Button {
                text = "Play"
                onAction = handle {
                  if (beatTimeline.isEmpty) {
                    durationSpinner.disable = true
                    val tl = Timeline(beats.map {
                      t =>
                        KeyFrame(Duration(t), onFinished = handle {
                          beatMediaPlayer.play()
                          beatMediaPlayer.seek(Duration(0))
                          Platform.runLater(beatsView.getSelectionModel.select(t))
                        })
                    })
                    beatTimeline = Some(tl)
                    tl.rate() = rateSlider.value()
                    player.rate() = rateSlider.value()
                    player.play()
                  }
                }
              },
              new Button {
                text = "beat"
                armed.onChange((_, _, x) => {
                  val t = player.getCurrentTime
                  if (!t.isUnknown && x)
                    insertBeat(t.toMillis)
                })
              },
              new Button {
                text = "Pause"
                onAction = handle {
                  player.pause()
                  beatTimeline.foreach(_.stop())
                  beatTimeline = None
                  valueFactory.value() = player.getCurrentTime.toMillis
                  durationSpinner.disable = false
                }

              },
              new Button {
                text = "Jump"
                onAction = handle {
                  val idx = beatsView.getSelectionModel.getSelectedIndex
                  if (idx != -1) {
                    player.seek(Duration(beats(idx)))
                  }

                }
              }
            )
          },
          rateSlider,
          new HBox {
            spacing = 5
            children = Seq(
              durationSpinner,
              duration,
              position
            )
          },
          new HBox {
            spacing = 5
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

