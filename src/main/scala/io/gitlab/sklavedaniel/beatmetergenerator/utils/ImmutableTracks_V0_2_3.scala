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

import scala.util.{Success, Try}
import ImmutableTracks_V0_2_0._

object ImmutableTracks_V0_2_3 {

  case class ImmutableTracks(content: List[ImmutableTrack], audio: Option[URI], flying: Boolean, flyingBeatmeter: FlyingBeatmeter.Conf_V0_2_3,
    waveformBeatmeter: WaveformBeatmeter.Conf_V0_2_0
  ) {
    def toMutable(base: URI, load: URI => Try[Array[Short]], undoManager: Option[UndoManager]): Try[Tracks] = {
      (audio match {
        case Some(uri) =>
          val auri = base.resolve(uri)
          load(auri).map(arr => Some((auri, arr)))
        case None =>
          Success(None)
      }).flatMap { aud =>
        val list = content.map(_.toMutable(base, load, undoManager)).foldLeft(Success(Nil): Try[List[Track]]) { (l, t) =>
          l.flatMap(l2 => t.map(t2 => t2 :: l2))
        }
        list.map { l =>
          val t = new Tracks(undoManager)
          undoManager.foreach(_.active = false)
          t.content ++= l.reverse
          t.audio() = aud
          t.flyingBeatmeter() = flyingBeatmeter
          t.waveformBeatmeter() = waveformBeatmeter
          t.flying() = flying
          undoManager.foreach(_.active = true)
          t
        }
      }
    }
  }

}
