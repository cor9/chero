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

import java.net.URI

import prickle._

import scala.collection.mutable
import scala.util.Try
import scalafx.scene.paint.Color

object JsonSerialization {

  implicit val pathPickler = new Pickler[URI] {
    override def pickle[P](obj: URI, state: PickleState)(implicit config: PConfig[P]): P = {
      config.makeString(obj.toString)
    }
  }
  implicit val pathUnpickler = new Unpickler[URI] {
    override def unpickle[P](pickle: P, state: mutable.Map[String, Any])(implicit config: PConfig[P]): Try[URI] = {
      config.readString(pickle).flatMap(s => Try(new URI(s)))
    }
  }
  implicit val colorPickler = new Pickler[Color] {
    override def pickle[P](obj: Color, state: PickleState)(implicit config: PConfig[P]): P = {
      config.makeString(s"${obj.red},${obj.green},${obj.blue},${obj.opacity}")
    }
  }
  implicit val colorUnpickler = new Unpickler[Color] {
    override def unpickle[P](pickle: P, state: mutable.Map[String, Any])(implicit config: PConfig[P]): Try[Color] = {
      config.readString(pickle).flatMap {s =>
        val Array(r,g,b,a) = s.split(',')
        Try(Color(r.toDouble, g.toDouble, b.toDouble, a.toDouble))}
    }
  }
  implicit val trackElementPickler: PicklerPair[ImmutableTrackElement] = CompositePickler[ImmutableTrackElement].concreteType[ImmutableBeat].concreteType[ImmutableMessage].
    concreteType[ImmutableBPMPattern].concreteType[ImmutableBeatsPattern]

  implicit val pickler: Pickler[ImmutableTrack] = Pickler.materializePickler[ImmutableTrack]
  implicit val unpickler: Unpickler[ImmutableTrack] = Unpickler.materializeUnpickler[ImmutableTrack]

  def save(tracks: ImmutableTracks): String = Pickle.intoString(tracks)

  def load(tracks: String) = Unpickle[ImmutableTracks].fromString(tracks)

}
