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
import scalafx.Includes._
import scalafx.animation.{Animation, KeyFrame, Timeline}
import scalafx.beans.binding.{Bindings, ObjectBinding}
import scalafx.beans.property.{DoubleProperty, ObjectProperty, ReadOnlyDoubleWrapper}
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
import scalafx.scene.input.DragEvent
import scalafx.scene.layout.{Pane, Priority, VBox}
import scalafx.util.Duration

import scala.collection.mutable

class TracksView(val tracks: Tracks, undoManager: UndoManager, player: AudioPlayer, digitDown: ObjectBinding[Option[Int]],
  trackHeadersBox: Pane
) extends ScrollPane {
  self =>

  val pxPerSec = DoubleProperty(0.0)
  val scale = DoubleProperty(1.0)
  val vwidth = Bindings.createDoubleBinding(() => viewportBounds().getWidth, viewportBounds)
  val visibleDuration = vwidth / pxPerSec
  private val tracksDuration_ = ReadOnlyDoubleWrapper(0.0)
  val tracksDuration = tracksDuration_.readOnlyProperty

  val totalDuration = Bindings.createDoubleBinding(() => ((player.duration.doubleValue() max player.position()._1 max tracksDuration()) + 10.0) max visibleDuration.doubleValue(), visibleDuration, player.duration, tracksDuration, player.position)
  val totalWidth = Bindings.createDoubleBinding(() => (pxPerSec() * ((player.duration.doubleValue() max player.position()._1 max tracksDuration()) + 10.0)) max vwidth.doubleValue(), vwidth, pxPerSec, player.duration, tracksDuration, player.position)
  val visiblePosition = hvalue * (totalDuration - visibleDuration)

  val snaps = ObjectProperty[Option[ObservableIntervalMap[Double, Beat]]](None)
  val record = ObjectProperty[Option[Track]](tracks.content.find(_.record()))

  val selectionContainer = ObjectProperty[Option[SelectionContainer[_]]](None)
  selectionContainer.onChange { (_, c, _) =>
    c.foreach(_.selectedElements.clear())
  }

  private val track2view = mutable.Map[Track, TrackView]()

  def getView(track: Track) = track2view(track)

  def views = track2view.values.toList

  private val track2headerView = mutable.Map[Track, TrackHeaderView]()

  vgrow = Priority.Sometimes

  override def requestFocus() {

  }

  focusTraversable = false

  private var scrollDelta: Double = 0.0

  val scrollTimeline = new Timeline {
    keyFrames = KeyFrame(time = Duration(50), onFinished = _ => {
      scrollX = 0.0.max(scrollX + scrollDelta)
    })
    cycleCount = Animation.Indefinite
  }

  val tracksBox = new VBox() {
    spacing = 5
  }

  private def calcTracksDuration(): Unit = {
    tracksDuration_() = (Iterator(0.0) ++ (for {
      track <- tracks.content
      (_, d, _) <- track.content.lastOption
    } yield d)).max
  }

  private val tracksListener: InvalidationListener = _ => {
    calcTracksDuration()
  }

  private def addTrack(i: Int, t: Track): Unit = {
    t.content.addListener(tracksListener)
    val v = new TrackView(20, t, snaps, undoManager, player.audio, digitDown, selectionContainer) {
      scaled <== scale
      duration <== totalDuration
    }
    val h = new TrackHeaderView(t, tracks, undoManager) {
      width <== trackHeadersBox.width
    }
    track2view(t) = v
    track2headerView(t) = h
    tracksBox.children.add(i, v)
    trackHeadersBox.children.add(i, h)
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
            track2headerView.remove(t).foreach(trackHeadersBox.children.remove)
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

  val contentPane = new Pane {
    maxWidth <== totalWidth
    prefWidth <== totalWidth
    minWidth <== totalWidth
    children = Seq(tracksBox)

    filterEvent(DragEvent.DragOver) { e: DragEvent =>
      if (e.getX < scrollX + 10) {
        scrollDelta = e.getX - scrollX - 10
        if (!Animation.Status.Running.equals(scrollTimeline.status())) {
          scrollTimeline.play()
        }
      } else if (e.getX > scrollX + viewportBounds().getWidth - 10) {
        scrollDelta = e.getX - scrollX - viewportBounds().getWidth + 10
        if (!Animation.Status.Running.equals(scrollTimeline.status())) {
          scrollTimeline.play()
        }
      } else if (Animation.Status.Running.equals(scrollTimeline.status())) {
        scrollTimeline.stop()
      }
    }
    filterEvent(DragEvent.DragDone) { e: DragEvent =>
      if (Animation.Status.Running.equals(scrollTimeline.status())) {
        scrollTimeline.stop()
      }
    }
  }

  content = contentPane

  def scrollX: Double = hvalue() * (contentPane.width.doubleValue() - viewportBounds().getWidth).max(0.0)

  def scrollX_=(x: Double): Unit = if (contentPane.width.doubleValue() - viewportBounds().getWidth > 0) {
    hvalue() = (x / (contentPane.width.doubleValue() - viewportBounds().getWidth)).max(hmin()).min(hmax())
  }

  def scrollY = vvalue() * (contentPane.height.doubleValue() - viewportBounds().getHeight).max(0.0)

  def scrollY_=(x: Double): Unit = if (contentPane.height.doubleValue() - viewportBounds().getHeight > 0) {
    vvalue() = (x / (contentPane.height.doubleValue() - viewportBounds().getHeight)).max(vmin()).min(vmax())
  }

}