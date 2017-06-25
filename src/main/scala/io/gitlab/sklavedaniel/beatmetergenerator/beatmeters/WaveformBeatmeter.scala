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
import shapeless.{HNil, :: => :::}

object WaveformBeatmeter {
  class Conf(base: BeatmeterBaseConf) extends BeatmeterSubcommand("waveform", base) {
    val foregroundImg = opt[Option[File]](descr = "Foreground svg image clipped to beatmeter width").map(_.map(_.toURI))
      .orElse(Some(Some(getClass.getResource("/meter/waveform/foreground.svg").toURI)))
    val startImg = opt[Option[File]](descr = "Left decoration svg image").map(_.map(_.toURI))
      .orElse(Some(Some(getClass.getResource("/meter/waveform/start.svg").toURI)))
    val endImg = opt[Option[File]](descr = "End decoration svg image").map(_.map(_.toURI))
      .orElse(Some(Some(getClass.getResource("/meter/waveform/end.svg").toURI)))
    val markerImg = opt[Option[File]](descr = "Target marker decoration svg image").map(_.map(_.toURI))
      .orElse(Some(Some(getClass.getResource("/meter/waveform/marker.svg").toURI)))
    val beatImg = opt[File](descr = "Beat svg image").map(_.toURI)
      .orElse(Some(getClass.getResource("/meter/waveform/beat.svg").toURI))
    val wavePattern = opt[Option[File ::: File ::: File ::: File ::: HNil]](descr = "Images for the wave line between beets given as --wave-pattern straightLine.svg waveStart.svg waveMiddle.Svg waveEnd.svg")
      .map(_.map(t => t.head.toURI :: t.tail.head.toURI :: t.tail.tail.head.toURI :: t.tail.tail.tail.head.toURI :: HNil))
      .orElse(Some(Some(getClass.getResource("/meter/waveform/waveStraight.svg").toURI ::
        getClass.getResource("/meter/waveform/waveStart.svg").toURI ::
        getClass.getResource("/meter/waveform/waveMiddle.svg").toURI ::
        getClass.getResource("/meter/waveform/waveEnd.svg").toURI :: HNil
      )))

    override def beatmeter(): Beatmeter = new WaveformBeatmeter(this)
  }
}
class WaveformBeatmeter(conf: WaveformBeatmeter.Conf) extends Beatmeter {

  val startPos: Double = conf.base.start.getOrElse(0.95)
  val endPos: Double = conf.base.end.getOrElse(0.05)

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
  val beatmeterForeground = conf.foregroundImg().map(getImage(_, conf.base.fgHeight(), imageCSS))
  val beatmeterMarker = conf.markerImg().map(getImage(_, conf.base.fgHeight(), imageCSS))
  val beatmeterStart = conf.startImg().map(getImage(_, conf.base.fgHeight(), imageCSS))
  val beatmeterEnd = conf.endImg().map(getImage(_, conf.base.fgHeight(), imageCSS))
  val beatmeterBeat = getImage(conf.beatImg(), conf.base.bmHeight(), imageCSS)
  val beatmeterWavePattern = conf.wavePattern().map(t => (
    getImage(t.head, conf.base.bmHeight(), imageCSS),
    getImage(t.tail.head, conf.base.bmHeight(), imageCSS), IndexedSeq((
    getImage(t.tail.tail.head, conf.base.bmHeight(), imageCSS),
    getImage(t.tail.tail.tail.head, conf.base.bmHeight(), imageCSS)))))

  override val minimalBeatDistance: Double = beatmeterBeat.getWidth

  override def getElementStreams(beats: Seq[Double], frameCount: Int): List[TimedStream] = {
    val beatmeterStream = {
      def line(x: Double, w: Double) = beatmeterWavePattern match {
        case Some(pattern) =>
          getImageLine(w.ceil.toInt, pattern).toPositioned(x, beatmeterY)
        case None => ElementStream[Positioned]()
      }

      val bs = beats.map(b => (b * conf.base.speed() + conf.base.target()) * conf.base.width() - beatmeterBeat.getWidth / 2)

      val start = line(endPos * conf.base.width(), (beats.head * conf.base.speed() + conf.base.target() - endPos) * conf.base.width() - beatmeterBeat.getWidth / 2.0)
      val middle = bs.sliding(2).map {
        case Seq(beat1, beat2) =>
          ElementStream(Positioned((beat1, beatmeterY), beatmeterBeat)) ++
            line(beat1 + beatmeterBeat.getWidth, beat2 - beat1 - beatmeterBeat.getWidth)
      }.reduceLeft(_ ++ _)
      val end = ElementStream(Positioned((bs.last, beatmeterY), beatmeterBeat)) ++
        line(bs.last + beatmeterBeat.getWidth, conf.base.duration() * conf.base.speed() * conf.base.width() - bs.last)

      start ++ middle ++ end
    }

    List(
      ElementStream(Fixed((0.0, 0.0), (_, g) => {
        g.setColor(conf.base.bgColor())
        g.fill(new geom.Rectangle2D.Double(conf.base.width() * endPos, beatmeterBackgroundY, conf.base.width() * startPos - conf.base.width() * endPos, conf.base.bgHeight()))
      }
      )).toTimed(),
      beatmeterStream.toTimed(conf.base.speed() * conf.base.width() / conf.base.frames(), conf.base.width())
        .clip(new geom.Rectangle2D.Double(conf.base.width() * endPos, beatmeterY, beatmeterWidth, conf.base.bmHeight())),
      ElementStream(
        beatmeterMarker.map(img =>
          Fixed((conf.base.target() * conf.base.width() - img.getWidth / 2.0, beatmeterForegroundY), img)
        ).toStream ++ beatmeterStart.map(img =>
          Fixed((startPos * conf.base.width() - img.getWidth / 2.0, beatmeterForegroundY), img)
        ).toStream ++ beatmeterEnd.map(img =>
          Fixed((endPos * conf.base.width() - img.getWidth / 2.0, beatmeterForegroundY), img)
        ).toStream
      ).toTimed(),
      ElementStream(
        beatmeterForeground.map(img =>
          Fixed((endPos * conf.base.width(), beatmeterForegroundY), img)
        ).toStream
      ).toTimed().clip(new geom.Rectangle2D.Double(endPos * conf.base.width(), beatmeterForegroundY, beatmeterWidth, conf.base.fgHeight()))
    )
  }

}
