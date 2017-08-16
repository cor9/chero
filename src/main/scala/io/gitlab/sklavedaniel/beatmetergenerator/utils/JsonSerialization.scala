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

import io.gitlab.sklavedaniel.beatmetergenerator.utils.ImmutableTracks_V0_2_0._
import microjson._
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
      config.readString(pickle).flatMap { s =>
        val Array(r, g, b, a) = s.split(',')
        Try(Color(r.toDouble, g.toDouble, b.toDouble, a.toDouble))
      }
    }
  }
  implicit val trackElementPickler: PicklerPair[ImmutableTrackElement] = CompositePickler[ImmutableTrackElement].concreteType[ImmutableBeat].concreteType[ImmutableMessage].
    concreteType[ImmutableBPMPattern].concreteType[ImmutableBeatsPattern]
  implicit val alignPickler: PicklerPair[Align] = CompositePickler[Align].concreteType[AlignLeft.type].concreteType[AlignRight.type].
    concreteType[AlignCenter.type]

  implicit val pickler: Pickler[ImmutableTrack] = Pickler.materializePickler[ImmutableTrack]
  implicit val unpickler: Unpickler[ImmutableTrack] = Unpickler.materializeUnpickler[ImmutableTrack]

  def save(tracks: ImmutableTracks): String = {
    val p = implicitly[Pickler[ImmutableTracks]]
    val data = p.pickle(tracks, PickleState())
    val json = JsObject(Map("version" -> JsString("0.2.0"), "data" -> data))
    Json.write(json)
  }

  def load(tracks: String) = {
    val json = Json.read(tracks)
    val data = Try {
      val JsObject(map) = json
      val JsString(version) = map("version")
      if ("0.2.0" != version) {
        throw new Exception("Unsupported version " + version)
      }
      map("data")
    }
    data.flatMap((d: JsValue) => Unpickle[ImmutableTracks].from(d))
  }

  def export(video: List[(Double, Boolean)], audio: List[(Double, Boolean, Option[URI])], messages: List[(Double, Double, String)]): String = {
    val config = JsConfig("#", false)
    val videoP = implicitly[Pickler[List[(Double, Boolean)]]]
    val audioP = implicitly[Pickler[List[(Double, Boolean, Option[URI])]]]
    val messagesP = implicitly[Pickler[List[(Double, Double, String)]]]
    val videoD = JsArray(video.map(v => JsObject(Map("time" -> JsNumber(v._1.toString), "highlighted" -> (if (v._2) JsTrue else JsFalse)))))
    val audioD = JsArray(audio.map(a => JsObject(Map("time" -> JsNumber(a._1.toString), "highlighted" -> (if (a._2) JsTrue else JsFalse)) ++
      a._3.map(uri => "sound" -> JsString(uri.toString)).toMap)))
    val messagesD = JsArray(messages.map(m => JsObject(Map("fromTime" -> JsNumber(m._1.toString), "toTime" -> JsNumber(m._2.toString), "message" -> JsString(m._3)))))
    val json = JsObject(Map("audio" -> audioD, "video" -> videoD, "messages" -> messagesD))
    Json.write(json)
  }

}
