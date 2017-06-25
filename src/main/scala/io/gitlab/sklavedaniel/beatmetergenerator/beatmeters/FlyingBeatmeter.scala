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

package io.gitlab.sklavedaniel.beatmetergenerator.beatmeters

import java.awt.geom
import java.awt.image.BufferedImage
import java.io.File

import io.gitlab.sklavedaniel.beatmetergenerator.beatmeters.Beatmeter._

object FlyingBeatmeter {

  class Conf(base: BeatmeterBaseConf) extends BeatmeterSubcommand("flying", base) {
    val beatImg = opt[File](descr = "Beat svg image").map(_.toURI)
      .orElse(Some(getClass.getResource("/meter/flying/beat.svg").toURI))
    val beatAnim = opt[File](descr = "Directory containing beat animation sequence").map(_.toURI)
      .orElse(Some(getClass.getResource("/meter/flying/beat.anim").toURI))

    override def beatmeter(): Beatmeter = new FlyingBeatmeter(this)
  }

}

class FlyingBeatmeter(conf: FlyingBeatmeter.Conf) extends Beatmeter {

  val startPos: Double = conf.base.start.getOrElse(1.0)
  val endPos: Double = conf.base.target()

  val beatmeterMiddle = conf.base.height() / 2.0
  val beatmeterWidth = (startPos - endPos) * conf.base.width()
  val beatmeterY = beatmeterMiddle - conf.base.bmHeight() / 2.0
  val beatmeterBackgroundY = beatmeterMiddle - conf.base.bgHeight() / 2.0
  val beatmeterForegroundY = beatmeterMiddle - conf.base.fgHeight() / 2.0

  val imageCSS =
    s"""
        .wave.fill {
          fill: ${getCSSColor(conf.base.wvColor())} !important;
          fill-opacity: ${getCSSOpacity(conf.base.wvColor())} !important;
        }
        .wave.stroke {
          stroke: ${getCSSColor(conf.base.wvColor())} !important;
          stroke-opacity: ${getCSSOpacity(conf.base.wvColor())} !important;
        }
        .foreground.fill {
          fill: ${getCSSColor(conf.base.fgColor())} !important;
          fill-opacity: ${getCSSOpacity(conf.base.fgColor())} !important;
        }
        .foreground.stroke {
          stroke: ${getCSSColor(conf.base.fgColor())} !important;
          stroke-opacity: ${getCSSOpacity(conf.base.fgColor())} !important;
        }
        .marker.fill {
          fill: ${getCSSColor(conf.base.mrColor())} !important;
          fill-opacity: ${getCSSOpacity(conf.base.mrColor())} !important;
        }
        .marker.stroke {
          stroke: ${getCSSColor(conf.base.mrColor())} !important;
          stroke-opacity: ${getCSSOpacity(conf.base.mrColor())} !important;
        }
    """

  val emptyWave = new BufferedImage(conf.base.bmHeight(), 1000, BufferedImage.TYPE_INT_ARGB)
  val beatmeterBeat = getImage(conf.beatImg(), conf.base.bmHeight(), imageCSS)
  val beatmeterAnim = getAnim(conf.beatAnim(), conf.base.bmHeight(), conf.base.frames(), imageCSS)

  override val minimalBeatDistance: Double = beatmeterBeat.getWidth

  override def getElementStreams(beats: Seq[Double], frameCount: Int): List[TimedStream] = {
    val beatmeterStream = {
      val bs = beats.map(b => (b * conf.base.speed() + conf.base.target()) * conf.base.width() - beatmeterBeat.getWidth / 2)

      val middle = bs.sliding(2).map {
        case Seq(beat1, beat2) =>
          ElementStream(Positioned((beat1, beatmeterY), beatmeterBeat))
      }.reduceLeft(_ ++ _)
      val end = ElementStream(Positioned((bs.last, beatmeterY), beatmeterBeat))

      middle ++ end
    }
    val beatAnimStream = ElementStream(beats.toStream.map(b => b * conf.base.frames()).map { b =>
      val frame = b.ceil.toInt
      Timed(frame, frame + beatmeterAnim._2.size, PositionDrawable(_ => (conf.base.target() * conf.base.width() - beatmeterAnim._2.head.getWidth / 2, beatmeterY),
        AnimDrawable(frame - b, beatmeterAnim)))
    })

    List(
      beatmeterStream.toTimed(conf.base.speed() * conf.base.width() / conf.base.frames(), endPos * conf.base.width(), beatmeterWidth),
      beatAnimStream
    )
  }

}
