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

import java.io.{File, FileOutputStream, IOException, OutputStreamWriter}
import java.net.URI

import io.gitlab.sklavedaniel.beatmetergenerator.editor.BeatEditor.{applicationSettings, applicationSettingsFile}
import upickle.default._
import ujson.Value

import scala.collection.mutable
import scala.util.{Failure, Try}
import scalafx.scene.paint.Color

import scala.io.Source

object JsonSerialization {

  implicit val pathReadWriter: ReadWriter[URI] =
    readwriter[String].bimap[URI](_.toString, new URI(_))

  implicit val colorReadWriter: ReadWriter[Color] =
    readwriter[String].bimap[Color]( c => {
      s"${c.red},${c.green},${c.blue},${c.opacity}"
    }, s => {
      val Array(r, g, b, a) = s.split(',')
      Color(r.toDouble, g.toDouble, b.toDouble, a.toDouble)
    })

  implicit val beatReadWriter: ReadWriter[ImmutableTracks_V0_2_0.ImmutableBeat] = macroRW
  implicit val messageReadWriter: ReadWriter[ImmutableTracks_V0_2_0.ImmutableMessage] = macroRW
  implicit val bpmPatternReadWriter: ReadWriter[ImmutableTracks_V0_2_0.ImmutableBPMPattern] = macroRW
  implicit val beatsPatternReadWriter: ReadWriter[ImmutableTracks_V0_2_0.ImmutableBeatsPattern] = macroRW
  implicit val trackElementReadWriter: ReadWriter[ImmutableTracks_V0_2_0.ImmutableTrackElement] =
    ReadWriter.merge(beatReadWriter, messageReadWriter, bpmPatternReadWriter, beatsPatternReadWriter)

  implicit val alignLeftReadWriter: ReadWriter[AlignLeft.type] = macroRW
  implicit val alignRightReadWriter: ReadWriter[AlignRight.type] = macroRW
  implicit val alignCenterReadWriter: ReadWriter[AlignCenter.type] = macroRW
  implicit val alignHReadWriter: ReadWriter[AlignH] = ReadWriter.merge(alignLeftReadWriter, alignRightReadWriter, alignCenterReadWriter)

  implicit val trackReadWriter: ReadWriter[ImmutableTracks_V0_2_0.ImmutableTrack] = macroRW
  implicit val tracksReadWriter_0_2_0: ReadWriter[ImmutableTracks_V0_2_0.ImmutableTracks] = macroRW
  implicit val tracksReadWriter_0_2_3: ReadWriter[ImmutableTracks_V0_2_3.ImmutableTracks] = macroRW
  implicit val applicationSettingsReadWriter: ReadWriter[ApplicationSettings] = macroRW

  implicit val flyingBeatmeterConf_0_2_0: ReadWriter[io.gitlab.sklavedaniel.beatmetergenerator.beatmeters.FlyingBeatmeter.Conf_V0_2_0] = macroRW
  implicit val flyingBeatmeterConf_0_2_3: ReadWriter[io.gitlab.sklavedaniel.beatmetergenerator.beatmeters.FlyingBeatmeter.Conf_V0_2_3] = macroRW
  implicit val wavefomrBeatmeterConf_0_2_3: ReadWriter[io.gitlab.sklavedaniel.beatmetergenerator.beatmeters.WaveformBeatmeter.Conf_V0_2_0] = macroRW

  def save(tracks: ImmutableTracks_V0_2_3.ImmutableTracks): String = {
    val data = writeJs(tracks)
    val json = ujson.Obj("version" -> ujson.Str("0.2.3"), "data" -> data)
    ujson.write(json)
  }

  def load(tracks: String): WithFailures[ImmutableTracks_V0_2_3.ImmutableTracks, Throwable] = {
    val json = ujson.read(tracks)
    val version = json("version").str
    version match {
      case "0.3.1" =>
        WithFailures.fromTry(Try(read[ImmutableTracks_V0_2_3.ImmutableTracks](json("data"))))
      case _ =>
        WithFailures.failure(new Exception("Unsupported version " + version))
    }
  }

  def export(video: List[(Double, Boolean)], audio: List[(Double, Boolean, Option[URI])], messages: List[(Double, Double, String)]): String = {
//    val videoP = implicitly[ReadWriter[List[(Double, Boolean)]]]
//    val audioP = implicitly[ReadWriter[List[(Double, Boolean, Option[URI])]]]
//    val messagesP = implicitly[ReadWriter[List[(Double, Double, String)]]]
    val videoD = ujson.Arr.from(video.map(v => ujson.Obj("time" -> ujson.Num(v._1), "highlighted" -> ujson.Bool(v._2))))
    val audioD = ujson.Arr.from(audio.map(a => ujson.Obj.from(Map("time" -> ujson.Num(a._1), "highlighted" -> ujson.Bool(a._2)) ++
      a._3.map(uri => "sound" -> ujson.Str(uri.toString)).toMap)))
    val messagesD = ujson.Arr.from(messages.map(m => ujson.Obj("fromTime" -> ujson.Num(m._1), "toTime" -> ujson.Num(m._2), "message" -> ujson.Str(m._3))))
    val json = ujson.Obj("audio" -> audioD, "video" -> videoD, "messages" -> messagesD)
    write(json)
  }

  def saveApplicationSettings(applicationSettings: ApplicationSettings, applicationSettingsFile: File): Unit = {
    try {
      applicationSettingsFile.getParentFile.mkdirs()
      val out = new OutputStreamWriter(new FileOutputStream(applicationSettingsFile), "utf-8")
      try {
        writeTo(applicationSettings, out)
      } finally {
        out.close()
      }
    } catch {
      case e: IOException =>
        System.err.println("Could not write application setting file: " + e.getMessage)
    }
  }

  def loadApplicationSettings(applicationSettingsFile: File): ApplicationSettings = {
    WithFailures.withFailures {
      val src = Source.fromFile(applicationSettingsFile, "utf-8")
      try {
        Try(read[ApplicationSettings](src.mkString)).toOption
      } finally {
        src.close()
      }
    }.value.flatten.getOrElse(new ApplicationSettings())
  }
}
