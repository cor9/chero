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

import io.gitlab.sklavedaniel.beatmetergenerator.editor.AudioPlayer.BeatInfo
import io.gitlab.sklavedaniel.beatmetergenerator.utils._
import javafx.beans.InvalidationListener
import scalafx.animation.{Animation, KeyFrame, Timeline}
import scalafx.beans.binding.{Bindings, ObjectBinding}
import scalafx.beans.property.{DoubleProperty, ObjectProperty, ReadOnlyDoubleWrapper}
import scalafx.collections.ObservableBuffer
import scalafx.scene.Group
import scalafx.scene.control.ScrollPane
import scalafx.scene.input.{DragEvent, MouseEvent, ScrollEvent}
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.util.Duration
import scalafx.Includes._

import scala.collection.mutable
import Utils._

class MainView(val tracks: Tracks, undoManager: UndoManager, player: AudioPlayer, digitDown: ObjectBinding[Option[Int]]) extends GridPane {

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

    val vwidth = Bindings.createDoubleBinding(() => viewportBounds().getWidth, viewportBounds)
    val waveView = new WaveView(20, 200, sceeneToContextX) {
      hvalue <== self.hvalue
      visibleWidth <== vwidth
      points <== player.maxima
      duration <== Bindings.createDoubleBinding(() => 10.0.max(player.audioDuration().max(tracksDuration())), player.duration, tracksDuration)
    }

    private var scrollDelta: Double = 0.0

    val scrollTimeline = new Timeline {
      keyFrames = KeyFrame(time = Duration(50), onFinished = _ => {
        scrollX = 0.0.max(scrollX + scrollDelta)
      })
      cycleCount = Animation.Indefinite
    }

    val tracksBox = new VBox {
      spacing = 5
    }

    val track2view = mutable.Map[Track, TrackView]()
    val track2headerView = mutable.Map[Track, TrackHeaderView]()

    private val tracksListener: InvalidationListener = _ => {
      calcTracksDuration()
    }

    private def addTrack(i: Int, t: Track): Unit = {
      t.content.addListener(tracksListener)
      val v = new TrackView(20, t, snaps, undoManager, player.audio, digitDown) {
        scaled <== waveView.scale
        duration <== Bindings.createDoubleBinding(() => 10.0.max(player.audioDuration().max(tracksDuration())), player.duration, tracksDuration)
      }
      val h = new TrackHeaderView(t, tracks, undoManager) {
        width <== headerBox.width
      }
      track2view(t) = v
      track2headerView(t) = h
      tracksBox.children.add(i, v)
      headerBox.children.add(i, h)
      val info = new BeatInfo()
      info.beat <== Bindings.createObjectBinding(() => {
        t.beat().map(_._2).getOrElse(AudioPlayer.defaultBeat)
      }, t.beat)
      player.beats.add(i, (h.track.play, info, v.beats))
      if (h.track.snap()) {
        snaps() = Some(v.beats)
      }
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
        waveView.scale() = (waveView.scale() * (1 + e.getDeltaY / 400)).max(0.25).min(40.0)
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

  val scale = scrollPane.waveView.scale
  val pxPerSec = scrollPane.waveView.pxPerSec

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

  add(headerGroup, 0, 0)
  add(scrollPane, 1, 0)

  columnConstraints = Seq(new ColumnConstraints(), new ColumnConstraints {
    hgrow = Priority.Always
    maxWidth = Double.PositiveInfinity
  })
  rowConstraints = Seq(new RowConstraints() {
    vgrow = Priority.Always
    maxHeight = Double.PositiveInfinity
  })
}