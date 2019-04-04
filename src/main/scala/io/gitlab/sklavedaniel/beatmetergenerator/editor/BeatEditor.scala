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

import java.awt.image.BufferedImage
import java.awt.{GraphicsEnvironment, RenderingHints, Shape, SplashScreen}
import java.io._
import java.net.URI
import java.nio.file.{Files, Paths}

import io.gitlab.sklavedaniel.beatmetergenerator._
import io.gitlab.sklavedaniel.beatmetergenerator.beatmeters.Beatmeter.Timed
import io.gitlab.sklavedaniel.beatmetergenerator.beatmeters.{Beatmeter, FlyingBeatmeter, WaveformBeatmeter}
import io.gitlab.sklavedaniel.beatmetergenerator.editor.Utils._
import io.gitlab.sklavedaniel.beatmetergenerator.utils.WithFailures._
import io.gitlab.sklavedaniel.beatmetergenerator.utils.{JsonSerialization, _}
import javax.imageio.ImageIO
import javax.sound.sampled.{AudioFileFormat, AudioInputStream, AudioSystem}
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.binding.Bindings
import scalafx.beans.property._
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{MenuItem, _}
import scalafx.scene.image.Image
import scalafx.scene.input._
import scalafx.scene.layout._
import scalafx.scene.text.{Font, Text}
import scalafx.stage.FileChooser.ExtensionFilter
import scalafx.stage.{DirectoryChooser, FileChooser}

import scala.collection.immutable.Queue
import scala.io.Source

object BeatEditor extends JFXApp {
  override def main(args: Array[String]): Unit = {
    System.setProperty("jdk.gtk.version", "2");
    super.main(args)
  }

  val courgette = java.awt.Font.createFont(java.awt.Font.TRUETYPE_FONT, getClass.getResourceAsStream("/Courgette-Regular.ttf"))
  GraphicsEnvironment.getLocalGraphicsEnvironment.registerFont(courgette)
  Font.loadFont(getClass.getResourceAsStream("/Courgette-Regular.ttf"), 20)
  val dataformat = new DataFormat("beats")

  Option(SplashScreen.getSplashScreen).foreach { splash =>
    val g = splash.createGraphics()
    g.setFont(courgette.deriveFont(20.0f))
    g.setColor(java.awt.Color.decode("0xff0896"))
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    val fb = g.getFontMetrics.getStringBounds(Information.version, g)
    val sb = splash.getBounds
    g.drawString(Information.version, sb.width - fb.getWidth.toFloat - 18, sb.height - fb.getHeight.toFloat - fb.getMinY.toFloat - 7)
    splash.update()
  }

  val applicationSettingsFile = Paths.get(System.getProperty("user.home")).resolve(".config").resolve("beatmeter_generator").resolve(Information.version).resolve("application.json")

  val applicationSettings = ObjectProperty(JsonSerialization.loadApplicationSettings(applicationSettingsFile.toFile))

  private val tracks = ObjectProperty(new Tracks(Some(new UndoManager())))

  val player = new AudioPlayer()

  private object Stage extends PrimaryStage {
    self =>
    title = "Beatmeter Generator"
    icons ++= Seq(16, 32, 64, 128).map(r => new Image(getClass.getResourceAsStream(s"/icon/icon-${r}x${r}.png")).delegate)
    val down = IndexedSeq.fill(10)(BooleanProperty(false))
    val digitDown = Bindings.createObjectBinding(() => down.zipWithIndex.map(d => if (d._1()) Some(d._2) else None).foldLeft(Option.empty[Int])(_.orElse(_)), down: _*)

    val mainView = Bindings.createObjectBinding(() => new MainView(player, tracks(), Stage.digitDown), tracks)

    val undoable = BooleanProperty(false)
    val redoable = BooleanProperty(false)

    def bindMainView(mv: MainView): Unit = {
      undoable <== mv.undoManager.undoable
      redoable <== mv.undoManager.redoable
    }

    mainView.onChange { (_, _, mv) =>
      bindMainView(mv)
    }
    bindMainView(mainView())

    scene = new Scene {
      filterEvent(KeyEvent.KeyPressed) { (e: KeyEvent) =>
        if (e.code == KeyCode.Digit0 || e.code == KeyCode.Numpad0) {
          down(0)() = true
        } else if (e.code == KeyCode.Digit1 || e.code == KeyCode.Numpad1) {
          down(1)() = true
        } else if (e.code == KeyCode.Digit2 || e.code == KeyCode.Numpad2) {
          down(2)() = true
        } else if (e.code == KeyCode.Digit3 || e.code == KeyCode.Numpad3) {
          down(3)() = true
        } else if (e.code == KeyCode.Digit4 || e.code == KeyCode.Numpad4) {
          down(4)() = true
        } else if (e.code == KeyCode.Digit5 || e.code == KeyCode.Numpad5) {
          down(5)() = true
        } else if (e.code == KeyCode.Digit6 || e.code == KeyCode.Numpad6) {
          down(6)() = true
        } else if (e.code == KeyCode.Digit7 || e.code == KeyCode.Numpad7) {
          down(7)() = true
        } else if (e.code == KeyCode.Digit8 || e.code == KeyCode.Numpad8) {
          down(8)() = true
        } else if (e.code == KeyCode.Digit9 || e.code == KeyCode.Numpad9) {
          down(9)() = true
        }
      }
      filterEvent(KeyEvent.KeyReleased) { (e: KeyEvent) =>
        if (e.code == KeyCode.Digit0 || e.code == KeyCode.Numpad0) {
          down(0)() = false
        } else if (e.code == KeyCode.Digit1 || e.code == KeyCode.Numpad1) {
          down(1)() = false
        } else if (e.code == KeyCode.Digit2 || e.code == KeyCode.Numpad2) {
          down(2)() = false
        } else if (e.code == KeyCode.Digit3 || e.code == KeyCode.Numpad3) {
          down(3)() = false
        } else if (e.code == KeyCode.Digit4 || e.code == KeyCode.Numpad4) {
          down(4)() = false
        } else if (e.code == KeyCode.Digit5 || e.code == KeyCode.Numpad5) {
          down(5)() = false
        } else if (e.code == KeyCode.Digit6 || e.code == KeyCode.Numpad6) {
          down(6)() = false
        } else if (e.code == KeyCode.Digit7 || e.code == KeyCode.Numpad7) {
          down(7)() = false
        } else if (e.code == KeyCode.Digit8 || e.code == KeyCode.Numpad8) {
          down(8)() = false
        } else if (e.code == KeyCode.Digit9 || e.code == KeyCode.Numpad9) {
          down(9)() = false
        }
      }

      def accessedFile(file: File): Unit = {
        val s = file.getAbsolutePath
        val tmp = (s :: applicationSettings().recentFiles.filterNot(_ == s)).take(20)
        applicationSettings() = applicationSettings().copy(recentFiles = tmp)
      }

      def save(oldFile: Option[File]): Unit = {
        val fc = new FileChooser()
        fc.title = "Beatmeter Generator: Save"
        tracks().file().foreach { f =>
          fc.initialDirectory = f.getParentFile
          fc.initialFileName = f.getName
        }
        oldFile.orElse(Option(fc.showSaveDialog(window()))) match {
          case Some(file) =>
            val task = (callback: (Option[Double], Option[String]) => Boolean) => {
              val s = JsonSerialization.save(tracks().toImmutable(file.getParentFile.toURI))
              val r = for (out <- resource.managed(new OutputStreamWriter(new FileOutputStream(file), "utf-8"))) yield {
                out.write(s)
              }
              fromTry(r.tried).map(Some(_))
            }
            val pd = new ProgressDialog[Unit](Some(mainView().scene().windowProperty()()), "Saving", Some("Saving..."), false, task)
            pd.showAndWait().get.asInstanceOf[WithFailures[Option[Unit], Throwable]] match {
              case WithFailures(Some(Some(_)), _) =>
              case WithFailures(None, e) =>
                alert("Could not save file", e.headOption.map(x => x.getLocalizedMessage).getOrElse(""), scene().windowProperty()())
              case WithFailures(Some(None), _) =>
            }
            tracks().file() = Some(file)
            accessedFile(file)
          case None =>
        }
      }

      def open(file: File): Unit = {
        try {
          val task = (callback: (Option[Double], Option[String]) => Boolean) => {
            val src = Source.fromFile(file, "utf-8")
            try {
              JsonSerialization.load(src.mkString).flatMap { ts =>
                ts.toMutable(file.getParentFile.toURI, (uri: URI) => {
                  withFailures(uri.toURL().openStream()).flatMap {
                    in => AudioPlayer.readData(new BufferedInputStream(in))
                  }
                }, Some(new UndoManager()))
              }.map(Some(_))
            } finally {
              src.close()
            }
          }
          val pd = new ProgressDialog[Tracks](Some(mainView().scene().windowProperty()()), "Loading", Some("Loading..."), false, task)
          pd.showAndWait().get.asInstanceOf[WithFailures[Option[Tracks], Throwable]] match {
            case WithFailures(Some(Some(ts)), e) =>
              ts.file() = Some(file)
              tracks() = ts
              if (e.nonEmpty) {
                alert("The following errors have been ignored", e.map(x => x.getLocalizedMessage).mkString("\n"), scene().windowProperty()())
              }
              accessedFile(file)
            case WithFailures(None, e) =>
              alert("Could not load file", e.headOption.map(x => x.getLocalizedMessage).getOrElse(""), scene().windowProperty()())
            case WithFailures(Some(None), _) =>
          }
        } catch {
          case e: Exception =>
            alert("Could not open file", e.getLocalizedMessage, scene().windowProperty()())
        }
      }

      root = new BorderPane {
        prefWidth = 800
        prefHeight = 600
        top = new MenuBar {
          menus = Seq(
            new Menu("File") {
              items = Seq(
                new MenuItem("Open") {
                  onAction = handle {
                    val fc = new FileChooser()
                    tracks().file().foreach { f =>
                      fc.initialDirectory = f.getParentFile
                    }
                    fc.title = "Beatmeter Generator: Open"
                    Option(fc.showOpenDialog(window())) match {
                      case Some(file) =>
                        open(file)
                      case None =>
                    }
                  }
                },
                new Menu("Open recent") {
                  disable <== Bindings.createBooleanBinding(() => applicationSettings().recentFiles.isEmpty, applicationSettings)

                  def computeItems(): Unit = {
                    items = applicationSettings().recentFiles.map(s => new MenuItem(s) {
                      onAction = handle {
                        open(new File(s))
                      }
                    })
                  }

                  computeItems()
                  applicationSettings.onChange { (_, _, _) =>
                    computeItems()
                  }
                },
                new MenuItem("Save as") {
                  onAction = handle {
                    save(None)
                  }
                },
                new MenuItem("Save") {
                  onAction = handle {
                    save(tracks().file())
                  }
                  accelerator = new KeyCodeCombination(KeyCode.S, KeyCombination.ControlDown)
                },
                new MenuItem("Load Audio") {
                  onAction = handle {
                    val fc = new FileChooser()
                    tracks().file().foreach { f =>
                      fc.initialDirectory = f.getParentFile
                    }
                    fc.title = "Beatmeter Generator: Open audio file"
                    fc.getExtensionFilters += new ExtensionFilter("wav audio file (16bit unsigned)", "*.wav")
                    Option(fc.showOpenDialog(window())) match {
                      case Some(file) =>
                        val task = (callback: (Option[Double], Option[String]) => Boolean) => {
                          AudioPlayer.readData(new BufferedInputStream(new FileInputStream(file))).map(Some(_))
                        }
                        val pd = new ProgressDialog[Array[Short]](Some(mainView().scene().windowProperty()()), "Loading audio", Some("Loading..."), false, task)
                        pd.showAndWait().get.asInstanceOf[WithFailures[Option[Array[Short]], Throwable]] match {
                          case WithFailures(Some(Some(data)), _) =>
                            tracks().audio() = Some((file.toURI, data))
                          case WithFailures(None, e) =>
                            alert("Could not load file", e.headOption.map(x => x.getLocalizedMessage).getOrElse(""), scene().windowProperty()())
                          case WithFailures(Some(None), _) =>
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
                    tracks().content += new Track(Some(mainView().undoManager))
                  }
                  accelerator = new KeyCodeCombination(KeyCode.T, KeyCombination.ControlDown)
                },
                new SeparatorMenuItem(),
                new MenuItem("Undo") {
                  disable <== !undoable
                  onAction = handle {
                    mainView().undoManager.undoAction()
                  }
                  accelerator = new KeyCodeCombination(KeyCode.Z, KeyCombination.ControlDown)
                },
                new MenuItem("Redo") {
                  disable <== !redoable
                  onAction = handle {
                    mainView().undoManager.redoAction()
                  }
                  accelerator = new KeyCodeCombination(KeyCode.Y, KeyCombination.ControlDown)
                },
                new SeparatorMenuItem(),
                new MenuItem("Copy") {
                  onAction = handle {
                    mainView().tracksView.selectionContainer().foreach { sc =>
                      sc.copySlected()
                    }
                  }
                  accelerator = new KeyCodeCombination(KeyCode.C, KeyCombination.ControlDown)
                },
                new MenuItem("Cut") {
                  onAction = handle {
                    mainView().tracksView.selectionContainer().foreach { sc =>
                      sc.copySlected()
                      sc.deleteSelectedElemts()
                    }
                  }
                  accelerator = new KeyCodeCombination(KeyCode.X, KeyCombination.ControlDown)
                },
                new MenuItem("Insert") {
                  onAction = handle {
                    mainView().tracksView.record().foreach { t =>
                      val v = mainView().tracksView.getView(t)
                      t.content(player.position()._1) match {
                        case Some((p, _, e)) =>
                          v.element2view((p, e)) match {
                            case sc: SelectionContainer[_] =>
                              sc.insert(player.position()._1 - sc.startPosition)
                            case _ =>
                          }
                        case None =>
                          v.insert(player.position()._1)
                      }
                    }
                  }
                  accelerator = new KeyCodeCombination(KeyCode.V, KeyCombination.ControlDown)
                },
                new MenuItem("Delete") {
                  onAction = handle {
                    mainView().tracksView.selectionContainer().foreach { sc =>
                      sc.deleteSelectedElemts()
                    }
                  }
                  accelerator = new KeyCodeCombination(KeyCode.Delete, KeyCombination.ControlDown)
                },
                new SeparatorMenuItem(),
                new MenuItem("Insert Beat") {
                  onAction = handle {
                    mainView().tracksView.record().foreach { track =>
                      val x = player.position()._1
                      if (track.content.intersecting(x, x + 0.05).isEmpty) {
                        track.content ++= Iterator((x, x + 0.05, new Beat(Some(mainView().undoManager))))
                      }
                    }
                  }
                  accelerator = new KeyCodeCombination(KeyCode.B, KeyCombination.ControlDown)
                },
                new MenuItem("Insert BPM Pattern") {
                  onAction = handle {
                    mainView().tracksView.record().foreach { track =>
                      val x = player.position()._1
                      val b = new BPMPattern(Some(mainView().undoManager))
                      mainView().undoManager.active = false
                      b.bpm() = 60
                      mainView().undoManager.active = true
                      mainView().tracksView.getView(track).newResizableElement(x, b)
                    }
                  }
                  accelerator = new KeyCodeCombination(KeyCode.P, KeyCombination.ControlDown)
                },
                new MenuItem("Insert Beat Pattern") {
                  onAction = handle {
                    mainView().tracksView.record().foreach { track =>
                      val x = player.position()._1
                      mainView().tracksView.getView(track).newBeatPattern(x, 1.0, Seq(0.0))
                    }
                  }
                  accelerator = new KeyCodeCombination(KeyCode.P, KeyCombination.ControlDown, KeyCombination.ShiftDown)
                },
                new MenuItem("Insert Message") {
                  onAction = handle {
                    mainView().tracksView.record().foreach { track =>
                      val x = player.position()._1
                      val b = new Message(Some(mainView().undoManager))
                      mainView().undoManager.active = false
                      b.text() = "Hello!"
                      mainView().undoManager.active = true
                      mainView().tracksView.getView(track).newResizableElement(x, b)
                    }
                  }
                  accelerator = new KeyCodeCombination(KeyCode.M, KeyCombination.ControlDown)
                }
              )
            },
            new Menu("Navigate") {
              items = Seq(
                new MenuItem("play/pause") {
                  onAction = handle {
                    player.playing() = !player.playing()
                  }
                  accelerator = new KeyCodeCombination(KeyCode.Space, KeyCombination.ControlDown)
                },
                new SeparatorMenuItem(),
                new MenuItem("Audio forward") {
                  onAction = handle {
                    player.position() = ((player.position()._1 + 1.0 / mainView().pxPerSec().doubleValue()).max(0.0), true)
                  }
                  accelerator = new KeyCodeCombination(KeyCode.Right, KeyCombination.ControlDown)
                },
                new MenuItem("Audio backward") {
                  onAction = handle {
                    player.position() = ((player.position()._1 - 1.0 / mainView().pxPerSec().doubleValue()).max(0.0), true)
                  }
                  accelerator = new KeyCodeCombination(KeyCode.Left, KeyCombination.ControlDown)
                },
                new MenuItem("Audio fast forward") {
                  onAction = handle {
                    player.position() = ((player.position()._1 + 10 * 1.0 / mainView().pxPerSec().doubleValue()).max(0.0), true)
                  }
                  accelerator = new KeyCodeCombination(KeyCode.Right, KeyCombination.ControlDown, KeyCombination.ShiftDown)
                },
                new MenuItem("Audio fast backward") {
                  onAction = handle {
                    player.position() = ((player.position()._1 - 10 * 1.0 / mainView().pxPerSec().doubleValue()).max(0.0), true)
                  }
                  accelerator = new KeyCodeCombination(KeyCode.Left, KeyCombination.ControlDown, KeyCombination.ShiftDown)
                },
                new MenuItem("Audio snap forward") {
                  onAction = handle {
                    mainView().tracksView.snaps().foreach { s =>
                      s.increase(player.position()._1).foreach { x =>
                        player.position() = (x._1.max(0.0)
                          , true
                        )
                      }
                    }
                  }
                  accelerator = new KeyCodeCombination(KeyCode.Right, KeyCombination.ControlDown, KeyCombination.MetaDown)
                },
                new MenuItem("Audio snap backward") {
                  onAction = handle {
                    mainView().tracksView.snaps().foreach { s =>
                      s.decrease(player.position()._1).foreach { x =>
                        player.position() = (x._1.max(0.0)
                          , true
                        )
                      }
                    }
                  }
                  accelerator = new KeyCodeCombination(KeyCode.Left, KeyCombination.ControlDown, KeyCombination.MetaDown)
                },
                new SeparatorMenuItem(),
                new MenuItem("To start") {
                  onAction = handle {
                    player.position() = (0.0, true)
                  }
                  accelerator = new KeyCodeCombination(KeyCode.Home, KeyCombination.ControlDown)
                },
                new MenuItem("To end") {
                  onAction = handle {
                    player.position() = (player.duration.doubleValue(), true)
                  }
                  accelerator = new KeyCodeCombination(KeyCode.End, KeyCombination.ControlDown)
                },
                new MenuItem("To audio end") {
                  onAction = handle {
                    player.position() = (player.audioDuration.doubleValue(), true)
                  }
                  accelerator = new KeyCodeCombination(KeyCode.End, KeyCombination.ControlDown, KeyCombination.ShiftDown)
                },
                new MenuItem("To beats end") {
                  onAction = handle {
                    player.position() = (player.beatsDuration.doubleValue(), true)
                  }
                  accelerator = new KeyCodeCombination(KeyCode.End, KeyCombination.ControlDown, KeyCombination.MetaDown)
                },
                new SeparatorMenuItem(),
                new MenuItem("Zoom in") {
                  onAction = handle {
                    val x = mainView().tracksView.width() / 2.0
                    val oldPosition = (mainView().tracksView.scrollX + x) / mainView().scale()
                    mainView().scale() = (mainView().scale() * (1 + 1.0 / 40)).max(0.25).min(40.0)
                    mainView().tracksView.scrollX = oldPosition * mainView().scale() - x
                  }
                  accelerator = new KeyCodeCombination(KeyCode.Plus, KeyCombination.ControlDown)
                },
                new MenuItem("Zoom out") {
                  onAction = handle {
                    val x = mainView().tracksView.width() / 2.0
                    val oldPosition = (mainView().tracksView.scrollX + x) / mainView().scale()
                    mainView().scale() = (mainView().scale() * (1 - 1.0 / 40)).max(0.25).min(40.0)
                    println(mainView().scale())
                    mainView().tracksView.scrollX = oldPosition * mainView().scale() - x
                  }
                  accelerator = new KeyCodeCombination(KeyCode.Minus, KeyCombination.ControlDown)
                })
            },
            new Menu("Tools") {
              items = Seq(
                new MenuItem("Generate Audio") {
                  onAction = handle {
                    val fc = new FileChooser()
                    tracks().file().foreach { f =>
                      fc.initialDirectory = f.getParentFile
                    }
                    fc.title = "Beatmeter Generator: Generate Audio"
                    Option(fc.showSaveDialog(window())) match {
                      case Some(file) =>
                        try {
                          val (is, count) = player.generateStream()
                          val task = (callback: (Option[Double], Option[String]) => Boolean) => {
                            val ais = new AudioInputStream(is(d => callback(Some(d), None)), AudioPlayer.format, count)
                            withFailures(try {
                              AudioSystem.write(ais, AudioFileFormat.Type.WAVE, file)
                            } finally {
                              ais.close()
                            }).map(_ => Some(()))
                          }
                          val pd = new ProgressDialog[Unit](Some(mainView().scene().windowProperty()()), "Generating audio", Some("Generating..."), true, task)
                          pd.showAndWait().get.asInstanceOf[WithFailures[Option[Unit], Throwable]] match {
                            case WithFailures(Some(Some(_)), _) =>
                            case WithFailures(None, e) =>
                              alert("Could not generate audio", e.headOption.map(x => x.getLocalizedMessage).getOrElse(""), scene().windowProperty()())
                            case WithFailures(Some(None), _) =>
                          }
                        } catch {
                          case e: Throwable =>
                            alert("Could not generate audio", e.getLocalizedMessage, scene().windowProperty()())
                        }
                      case None =>
                    }


                  }
                },
                new MenuItem("Generate Video") {
                  onAction = handle {

                    val beats = merge[(Double, Boolean), Double](mainView().tracksView.views.toList.filter(_.track.display()).map(_.beats.toList.map(b => (b._1, b._3.highlight()))),
                      b => b._1)
                    val messages = merge[(Double, Double, String), Double](
                      tracks().content.toList.filter(_.display()).map(_.content.toList.flatMap { b =>
                        b._3 match {
                          case msg: Message => Some((b._1, b._2, msg.text()))
                          case _ => None
                        }
                      }),
                      b => b._1)

                    if (beats.nonEmpty || messages.nonEmpty) {
                      val beatmeter: Beatmeter = if (tracks().flying()) {
                        new FlyingBeatmeter(tracks().flyingBeatmeter())
                      } else {
                        new WaveformBeatmeter(tracks().waveformBeatmeter())
                      }
                      val beatViolations = if (beats.size < 2) {
                        Nil
                      } else {
                        beats.sliding(2).filter { case Seq(a, b) => b._1 - a._1 < beatmeter.minimalBeatDistance }
                      }
                      val messageViolations = if (messages.size < 2) {
                        Nil
                      } else {
                        messages.sliding(2).filter { case Seq(a, b) => a._2 >= b._1 }
                      }
                      if (beatViolations.nonEmpty) {
                        alert(s"Beat Distance is smaller than ${beatmeter.minimalBeatDistance}s.", beatViolations.map { case Seq(a, b) => s"${a._1} ${b._1}" }.mkString("\n"), scene().windowProperty()())
                      } else if (messageViolations.nonEmpty) {
                        alert("Messages are overlapping.", messageViolations.map { case Seq(a, b) => s"${a._1} ${b._1}" }.mkString("\n"), scene().windowProperty()())
                      } else {
                        val frameCount = (beatmeter.frames * player.duration.doubleValue()).round.toInt

                        class State(val clip: Option[Shape], var remaining: Stream[Timed]) {
                          var current: Queue[Timed] = Queue()
                        }

                        val states: List[State] = beatmeter.getElementStreams(beats, messages, frameCount).map(tl => new State(tl.clip, tl.stream))

                        val fc = new DirectoryChooser()
                        tracks().file().foreach { f =>
                          fc.initialDirectory = f.getParentFile
                        }
                        fc.title = "Beatmeter Generator: Generate Video"
                        Option(fc.showDialog(window())) match {
                          case Some(dir) =>
                            try {
                              val cnt = if (dir.exists() && dir.listFiles().nonEmpty) {
                                val alert = new Alert(AlertType.Confirmation) {
                                  title = "Beatmeter Generator"
                                  headerText = "Output Directory is not empty!"
                                  dialogPane().content = new Text(s"Delete all contents of\n${dir.getAbsolutePath}?") {
                                    wrappingWidth = 500
                                  }
                                }

                                def deleteDir(file: File): Unit = {
                                  if (file.isDirectory) {
                                    file.listFiles().foreach(deleteDir)
                                  }
                                  if (!file.delete()) {
                                    throw new Exception(s"Could not delete ${file.getAbsolutePath}.")
                                  }
                                }

                                alert.initOwner(mainView().scene().windowProperty()())
                                if (ButtonType.OK == alert.showAndWait().get) {
                                  dir.listFiles().foreach(deleteDir)
                                  true
                                } else {
                                  false
                                }
                              } else {
                                true
                              }
                              if (cnt) {
                                Files.createDirectories(dir.toPath)
                                val task = (callback: (Option[Double], Option[String]) => Boolean) => {
                                  var compute = true
                                  withFailures(for (i <- 0 until frameCount) {
                                    if (compute) {
                                      compute = callback(Some((i + 1).toDouble / frameCount), Some(s"Encoding frame ${i + 1} of $frameCount"))
                                      val currentTime = i.toDouble / beatmeter.frames
                                      val image = new BufferedImage(beatmeter.width, beatmeter.height, BufferedImage.TYPE_INT_ARGB)
                                      val g = image.createGraphics()
                                      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                                        RenderingHints.VALUE_ANTIALIAS_ON)

                                      for (state <- states) {
                                        state.current = state.current.filter { case Timed(_, endFrame, _) => endFrame > i }
                                          .enqueue(state.remaining.takeWhile { case Timed(startFrame, _, _) => startFrame <= i })
                                        state.remaining = state.remaining.dropWhile { case Timed(startFrame, _, _) => startFrame <= i }
                                        state.clip.foreach(g.setClip)

                                        for (Timed(startFrame, _, drawable) <- state.current) {
                                          val offset = i - startFrame
                                          drawable.draw(offset, g)
                                        }

                                        g.setClip(null) // scalastyle:ignore null

                                      }

                                      ImageIO.write(image, "PNG", new File(dir, f"frame-$i%010d.png"))
                                    }
                                  }).map(Some(_))
                                }
                                val pd = new ProgressDialog[Unit](Some(mainView().scene().windowProperty()()), "Generating Video", Some("Generating..."), true, task)
                                pd.showAndWait().get.asInstanceOf[WithFailures[Option[Unit], Throwable]] match {
                                  case WithFailures(Some(Some(_)), _) =>
                                  case WithFailures(None, e) =>
                                    alert("Could not generate video", e.headOption.map(x => x.getLocalizedMessage).getOrElse(""), scene().windowProperty()())
                                  case WithFailures(Some(None), _) =>
                                }

                              }
                            } catch {
                              case e: Throwable =>
                                alert("Could not generate video", e.getLocalizedMessage, scene().windowProperty()())
                            }
                          case None =>
                        }
                      }
                    }
                  }
                },
                new MenuItem("Beatmeter Settings") {
                  onAction = handle {
                    val dialog = new BeatmeterDialog(Some(mainView().scene().windowProperty()()), tracks().flying(),
                      tracks().flyingBeatmeter(), tracks().waveformBeatmeter(), tracks().file().map(_.getParentFile))
                    val r = dialog.showAndWait().get.asInstanceOf[(Boolean, FlyingBeatmeter.Conf_V0_2_3, WaveformBeatmeter.Conf_V0_2_0)]
                    mainView().undoManager.startGroup()
                    tracks().flying() = r._1
                    tracks().flyingBeatmeter() = r._2
                    tracks().waveformBeatmeter() = r._3
                    mainView().undoManager.endGroup()
                  }
                },
                new MenuItem("Export Beats and Messages") {
                  onAction = handle {
                    val videoBeats = merge[(Double, Boolean), Double](mainView().tracksView.views.filter(_.track.display()).map(_.beats.toList.map(b => (b._1, b._3.highlight()))),
                      b => b._1)
                    val audioBeats = merge[(Double, Boolean, Option[URI]), Double](mainView().tracksView.views.filter(_.track.play()).map(t => t.beats.toList.map(b => (b._1, b._3.highlight(), t.track.beat().map(_._1)))),
                      b => b._1)
                    val messages = merge[(Double, Double, String), Double](
                      tracks().content.toList.filter(_.display()).map(_.content.toList.flatMap { b =>
                        b._3 match {
                          case msg: Message => Some((b._1, b._2, msg.text()))
                          case _ => None
                        }
                      }),
                      b => b._1)
                    val fc = new FileChooser()
                    tracks().file().foreach { f =>
                      fc.initialDirectory = f.getParentFile
                    }
                    fc.title = "Beatmeter Generator: Export audio and messages"
                    Option(fc.showSaveDialog(window())) match {
                      case Some(file) =>
                        val task = (callback: (Option[Double], Option[String]) => Boolean) => {
                          val s = JsonSerialization.export(videoBeats, audioBeats, messages)
                          val r = for (out <- resource.managed(new OutputStreamWriter(new FileOutputStream(file), "utf-8"))) yield {
                            out.write(s)
                          }
                          fromTry(r.tried).map(Some(_))
                        }
                        val pd = new ProgressDialog[Unit](Some(mainView().scene().windowProperty()()), "Saving", Some("Saving..."), false, task)
                        pd.showAndWait().get.asInstanceOf[WithFailures[Option[Unit], Throwable]] match {
                          case WithFailures(Some(Some(_)), _) =>
                          case WithFailures(None, e) =>
                            alert("Could not save file", e.headOption.map(x => x.getLocalizedMessage).getOrElse(""), scene().windowProperty()())
                          case WithFailures(Some(None), _) =>
                        }
                      case None =>
                    }
                  }
                }
              )
            },
            new Menu("Info") {
              items = Seq(
                new MenuItem("About") {
                  onAction = handle {
                    val alert = new AboutDialog(hostServices)
                    alert.initOwner(mainView().scene().windowProperty()())
                    alert.showAndWait()
                  }
                },
                new MenuItem("License") {
                  onAction = handle {
                    val alert = new LicenseDialog(hostServices)
                    alert.initOwner(mainView().scene().windowProperty()())
                    alert.showAndWait()
                  }
                }
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
                mainView().tracksView.record().foreach { track =>
                  val x = player.position()._1
                  if (track.content.intersecting(x, x + 0.05).isEmpty) {
                    track.content ++= Iterator((x, x + 0.05, new Beat(Some(mainView().undoManager))))
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

              add(new Label("Position") {
                tooltip = Tooltip("Current player position in mm:ss.ms")
              }, 0, 0)
              add(new Label {
                text <== Bindings.createStringBinding(() => formatTime(player.position()._1), player.position)
              }, 1, 0)
              add(new Label("Duration") {
                tooltip = Tooltip("Current player position in mm:ss.ms")
              }, 0, 1)
              add(new Label {
                text <== Bindings.createStringBinding(() => formatTime(player.duration.doubleValue()), player.duration)
              }, 1, 1)
            },
            new GridPane {
              scaleX = 0.8
              scaleY = 0.8
              hgap = 5
              vgap = 5
              add(new Label("Speed") {
                tooltip = Tooltip("Playback speed")
              }, 0, 0)
              add(new Slider(0.2f, 1.0f, 1.0f) {
                blockIncrement = 0.1f
                value <==> player.rate
              }, 1, 0)
              add(new Label("Volume") {
                tooltip = Tooltip("Ratio of volume between beats and audio file")
              }, 0, 1)
              add(new Slider(0.0, 1.0, 0.5) {
                blockIncrement = 0.1
                value <==> player.ratio
              }, 1, 1)
            }
          )
        }
      }
    }
    onShown = handle {
      Option(SplashScreen.getSplashScreen).foreach { splash =>
        splash.close()
      }
    }
  }

  stage = Stage

  player.progressDialogWindow() = Some(stage.scene().getWindow)

  override def stopApp(): Unit = {
    JsonSerialization.saveApplicationSettings(applicationSettings(), applicationSettingsFile.toFile)
  }
}
