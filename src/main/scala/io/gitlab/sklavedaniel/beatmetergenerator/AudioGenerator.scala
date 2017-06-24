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

package io.gitlab.sklavedaniel.beatmetergenerator

import java.io.{File, InputStream}
import javax.sound.sampled._

import org.apache.commons.io.IOUtils
import org.rogach.scallop.ScallopConf

object AudioGenerator {

  class Conf extends Main.ExecutableSubcommand("audio") {
    val input = opt[File](required = true, descr = "File containing beat definitions")
    validateFileExists(input)
    val output = opt[File](required = true, descr = "Generated wav file")
    validateFileDoesNotExist(output)
    val beat = opt[File](descr = "Single beat wav file")
    validateFileExists(beat)
    val duration = opt[Double](required = true, descr = "Duration of generated wav file in seconds")
    val click = opt[Boolean](default = Some(false), descr = "Use click sound")
    override def execute(subcommands: List[ScallopConf], args: Array[String]) = new AudioGenerator(this).main(args)
  }

}

class AudioGenerator(conf: AudioGenerator.Conf) extends App {

  val beats: Seq[Double] = BeatFiles.load(conf.input())

  val ais = AudioSystem.getAudioInputStream(conf.beat.map(_.toURI.toURL).getOrElse(getClass.getResource(if(conf.click()) "/beats/click.wav" else "/beats/beat.wav")))
  val format = ais.getFormat
  val frameCount = (conf.duration() * format.getFrameRate).ceil.toInt
  val bytes = IOUtils.toByteArray(ais)
  val silence = Stream.continually(Array.fill(format.getFrameSize)(0.toByte))
  var beat: Stream[Array[Byte]] = bytes.grouped(format.getFrameSize).toStream ++ silence

  val itr: Iterator[Byte] = silence.slice(0, (beats.head * format.getSampleRate).round.toInt).toIterator.flatten ++
    beats.sliding(2).filter(_.size == 2).zipWithIndex.flatMap {
      case (Seq(beat1, beat2), i) =>
        println(s"Generating beat ${i + 2} of ${beats.size}")
        beat.slice(0, ((beat2 - beat1) * ais.getFormat.getSampleRate).round.toInt).toIterator.flatten
    } ++ beat.slice(0, ((conf.duration() - beats.last) * ais.getFormat.getSampleRate).round.toInt).toIterator.flatten

  val is = new InputStream {
    override def read(): Int = if (itr.hasNext) itr.next & 0xff else -1
  }

  val out = new AudioInputStream(is, ais.getFormat, frameCount)
  println(s"Generating beat 1 of ${beats.size}")
  AudioSystem.write(out, AudioFileFormat.Type.WAVE, conf.output())
  out.close()
}
