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

package io.gitlab.sklavedaniel.beatmetergenerator.utils

import java.net.URI

import io.gitlab.sklavedaniel.beatmetergenerator.beatmeters.{FlyingBeatmeter, WaveformBeatmeter}

import WithFailures._

object ImmutableTracks_V0_2_0 {

  case class ImmutableTracks(content: List[ImmutableTrack], audio: Option[URI], flying: Boolean, flyingBeatmeter: FlyingBeatmeter.Conf_V0_2_0,
    waveformBeatmeter: WaveformBeatmeter.Conf_V0_2_0
  ) {
    def toV_0_2_3 = ImmutableTracks_V0_2_3.ImmutableTracks(content, audio, flying, flyingBeatmeter.toV0_2_3, waveformBeatmeter)
  }

  case class ImmutableTrack(title: String, play: Boolean, record: Boolean, display: Boolean, snap: Boolean,
    content: List[(Double, Double, ImmutableTrackElement)], beat: Option[URI]
  ) {
    def toMutable(base: URI, load: URI => WithFailures[Array[Short], Throwable], undoManager: Option[UndoManager]) = {
      (beat match {
        case Some(uri) =>
          val auri = base.resolve(uri)
          load(auri).map(arr => Some((auri, arr)))
        case None =>
          success(None)
      }).map { aud =>
        val tmp = new Track(undoManager)
        val cntnt = content.map { elem =>
          (elem._1, elem._2, elem._3.toMutable(undoManager))
        }
        undoManager.foreach(_.active = false)
        tmp.title() = title
        tmp.play() = play
        tmp.record() = record
        tmp.display() = display
        tmp.snap() = snap
        tmp.content ++= cntnt
        tmp.beat() = aud
        undoManager.foreach(_.active = true)
        tmp
      }
    }
  }


  sealed trait ImmutableTrackElement {
    def toMutable(undoManager: Option[UndoManager]): TrackElement
  }

  case class ImmutableBeat(highlight: Boolean) extends ImmutableTrackElement {
    override def toMutable(undoManager: Option[UndoManager]) = {
      val b = new Beat(undoManager)
      undoManager.foreach(_.active = false)
      b.highlight() = highlight
      undoManager.foreach(_.active = true)
      b
    }
  }

  case class ImmutableBPMPattern(highlightFirst: Boolean, bpm: Double, beat: ImmutableBeat) extends ImmutableTrackElement {
    override def toMutable(undoManager: Option[UndoManager]) = {
      val b = new BPMPattern(beat.toMutable(undoManager), undoManager)
      undoManager.foreach(_.active = false)
      b.bpm() = bpm
      b.highlightFirst() = highlightFirst
      undoManager.foreach(_.active = true)
      b
    }
  }

  case class ImmutableBeatsPattern(highlightFirstPattern: Boolean, highlightFirst: Boolean, patternDuration: Double, pattern: List[(Double, Double, ImmutableBeat)]) extends ImmutableTrackElement {
    override def toMutable(undoManager: Option[UndoManager]) = {
      val b = new BeatsPattern(undoManager)
      val pttrn = pattern.map(x => (x._1, x._2, x._3.toMutable(undoManager)))
      undoManager.foreach(_.active = false)
      b.highlightFirstPattern() = highlightFirstPattern
      b.highlightFirst() = highlightFirst
      b.patternDuration() = patternDuration
      b.pattern ++= pttrn
      undoManager.foreach(_.active = true)
      b
    }
  }

  case class ImmutableMessage(text: String) extends ImmutableTrackElement {
    override def toMutable(undoManager: Option[UndoManager]) = {
      val b = new Message(undoManager)
      undoManager.foreach(_.active = false)
      b.text() = text
      undoManager.foreach(_.active = true)
      b
    }
  }

}
