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

import java.awt.Shape
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import io.gitlab.sklavedaniel.beatmetergenerator.beatmeters.Beatmeter._
import io.gitlab.sklavedaniel.beatmetergenerator.beatmeters._
import org.rogach.scallop.ScallopConf
import shapeless.{:: => :::}

import scala.collection.immutable.Queue

object VideoGenerator {

  class Conf extends Main.ExecutableSubcommand("video") with BeatmeterBaseConf {
    val input = opt[File](required = true, descr = "File containing beat definitions")
    validateFileExists(input)
    val output = opt[File](required = true, descr = "Directory for generated images")
    validateFileDoesNotExist(output)

    val beatmeters: Seq[BeatmeterSubcommand] = Seq(
      new WaveformBeatmeter.Conf(this),
      new FlyingBeatmeter.Conf(this)
    )
    for (bm <- beatmeters)
      addSubcommand(bm)

    override def execute(subcommands: List[ScallopConf], args: Array[String]) = new VideoGenerator(subcommands, this).main(args)
  }

}

class VideoGenerator(subcommands: List[ScallopConf], conf: VideoGenerator.Conf) extends App {

  val beatmeter = subcommands.headOption.getOrElse(conf.beatmeters.head).asInstanceOf[BeatmeterSubcommand].beatmeter()

  println(s"Heights: video: ${conf.height()}, beatmeter: ${conf.bmHeight()}, foreground: ${conf.bmHeight()}, background: ${conf.bmHeight()}")

  val beats: Seq[Double] = BeatFiles.load(conf.input())
  require(beats.size >= 2, "You need at least two beats.")

  val frameCount = (conf.frames() * conf.duration()).round.toInt

  val minBeatDistance = beatmeter.minimalBeatDistance
  val beatDistances = beats.sliding(2).flatMap {
    case Seq(beat1, beat2) =>
      Some((beat1, beat2, (beat2 - beat1) * conf.speed() * conf.width())).filter(_._3 < minBeatDistance)
  }.toStream
  if (beatDistances.nonEmpty) {
    println(s"Error: The following beats have distance below ${minBeatDistance / conf.speed() / conf.width()}:")
    for ((beat1, beat2, _) <- beatDistances) {
      println(s"    $beat1 $beat2")
    }
    println("Increase beatmeter speed or beat distance")
    System.exit(1)
  }

  class State(val clip: Option[Shape], var remaining: Stream[Timed]) {
    var current: Queue[Timed] = Queue()
  }

  val elementStreams = beatmeter.getElementStreams(beats, frameCount)

  val states: List[State] = elementStreams.map(tl => new State(tl.clip, tl.stream))

  conf.output().mkdirs()
  for (i <- 0 until frameCount) {
    println(s"Encoding frame ${i + 1} of $frameCount")
    val currentTime = i.toDouble / conf.frames()
    val image = new BufferedImage(conf.width(), conf.height(), BufferedImage.TYPE_INT_ARGB)
    val g = image.createGraphics()

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

    ImageIO.write(image, "PNG", new File(conf.output(), f"frame-$i%010d.png"))
  }

  println(s"Heights: video: ${conf.height()}, beatmeter: ${conf.bmHeight()}, foreground: ${conf.bmHeight()}, background: ${conf.bmHeight()}")

}
