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

import java.awt.{BasicStroke, Font, RenderingHints, geom}
import java.net.URI

import io.gitlab.sklavedaniel.beatmetergenerator.beatmeters.Beatmeter._
import io.gitlab.sklavedaniel.beatmetergenerator.utils.{AlignH, AlignCenter, AlignLeft, AlignRight}

import scalafx.scene.paint.Color

object WaveformBeatmeter {

  case class Conf_V0_2_0(width: Int, height: Int, frames: Double, speed: Double, position: Double, startPosition: Double, endPosition: Double,
    waveColor: Color, waveHighlightedColor: Color, foregroundColor: Color, backgroundColor: Color, markerColor: Color,
    messageFont: (String, Int, Boolean, Boolean), margin: Int, messageColor: Color, messageBorderColor: Color,
    messageBorderStrength: Double, messageAlign: AlignH, messagePosition: Double, imageDirectory: Option[URI]
  )

}

class WaveformBeatmeter(conf: WaveformBeatmeter.Conf_V0_2_0) extends Beatmeter {

  val startPos: Double = conf.startPosition
  val endPos: Double = conf.endPosition

  val beatmeterWidth: Double = (startPos - endPos) * conf.width
  val beatmeterY: Double = conf.margin + conf.messageFont._2
  val beatmeterMiddle: Double = beatmeterY + conf.height / 2.0

  override val height = conf.height + conf.margin + conf.messageFont._2
  override val width = conf.width
  override val frames = conf.frames

  val imageCSS =
    s"""
        .wave.fill {
          fill: ${getCSSColor(conf.waveColor)} !important;
          fill-opacity: ${getCSSOpacity(conf.waveColor)} !important;
        }
        .wave.stroke {
          stroke: ${getCSSColor(conf.waveColor)} !important;
          stroke-opacity: ${getCSSOpacity(conf.waveColor)} !important;
        }
        .foreground.fill {
          fill: ${getCSSColor(conf.foregroundColor)} !important;
          fill-opacity: ${getCSSOpacity(conf.foregroundColor)} !important;
        }
        .foreground.stroke {
          stroke: ${getCSSColor(conf.foregroundColor)} !important;
          stroke-opacity: ${getCSSOpacity(conf.foregroundColor)} !important;
        }
        .background.fill {
          fill: ${getCSSColor(conf.backgroundColor)} !important;
          fill-opacity: ${getCSSOpacity(conf.backgroundColor)} !important;
        }
        .background.stroke {
          stroke: ${getCSSColor(conf.backgroundColor)} !important;
          stroke-opacity: ${getCSSOpacity(conf.backgroundColor)} !important;
        }
        .marker.fill {
          fill: ${getCSSColor(conf.markerColor)} !important;
          fill-opacity: ${getCSSOpacity(conf.markerColor)} !important;
        }
        .marker.stroke {
          stroke: ${getCSSColor(conf.markerColor)} !important;
          stroke-opacity: ${conf.markerColor} !important;
        }
    """

  val imageHighlightedCSS =
    s"""
        .wave.fill {
          fill: ${getCSSColor(conf.waveHighlightedColor)} !important;
          fill-opacity: ${getCSSOpacity(conf.waveHighlightedColor)} !important;
        }
        .wave.stroke {
          stroke: ${getCSSColor(conf.waveHighlightedColor)} !important;
          stroke-opacity: ${getCSSOpacity(conf.waveHighlightedColor)} !important;
        }
        .foreground.fill {
          fill: ${getCSSColor(conf.foregroundColor)} !important;
          fill-opacity: ${getCSSOpacity(conf.foregroundColor)} !important;
        }
        .foreground.stroke {
          stroke: ${getCSSColor(conf.foregroundColor)} !important;
          stroke-opacity: ${getCSSOpacity(conf.foregroundColor)} !important;
        }
        .background.fill {
          fill: ${getCSSColor(conf.backgroundColor)} !important;
          fill-opacity: ${getCSSOpacity(conf.backgroundColor)} !important;
        }
        .background.stroke {
          stroke: ${getCSSColor(conf.backgroundColor)} !important;
          stroke-opacity: ${getCSSOpacity(conf.backgroundColor)} !important;
        }
        .marker.fill {
          fill: ${getCSSColor(conf.markerColor)} !important;
          fill-opacity: ${getCSSOpacity(conf.markerColor)} !important;
        }
        .marker.stroke {
          stroke: ${getCSSColor(conf.markerColor)} !important;
          stroke-opacity: ${conf.markerColor} !important;
        }
    """

  val beatmeterForegroundURI = conf.imageDirectory.map(_.resolve("foreground.svg")).getOrElse(getClass.getResource("/meter/waveform/foreground.svg").toURI)
  val beatmeterForeground = getImage(beatmeterForegroundURI, conf.height, imageCSS)
  val beatmeterBackgroundURI = conf.imageDirectory.map(_.resolve("background.svg")).getOrElse(getClass.getResource("/meter/waveform/background.svg").toURI)
  val beatmeterBackground = getImage(beatmeterBackgroundURI, conf.height, imageCSS)
  val beatmeterMarkerURI = conf.imageDirectory.map(_.resolve("marker.svg")).getOrElse(getClass.getResource("/meter/waveform/marker.svg").toURI)
  val beatmeterMarker = getImage(beatmeterMarkerURI, conf.height, imageCSS)
  val beatmeterStartURI = conf.imageDirectory.map(_.resolve("start.svg")).getOrElse(getClass.getResource("/meter/waveform/start.svg").toURI)
  val beatmeterStart = getImage(beatmeterStartURI, conf.height, imageCSS)
  val beatmeterEndURI = conf.imageDirectory.map(_.resolve("end.svg")).getOrElse(getClass.getResource("/meter/waveform/end.svg").toURI)
  val beatmeterEnd = getImage(beatmeterEndURI, conf.height, imageCSS)
  val beatmeterBeatURI = conf.imageDirectory.map(_.resolve("beat.svg")).getOrElse(getClass.getResource("/meter/waveform/beat.svg").toURI)
  val beatmeterBeat = getImage(beatmeterBeatURI, conf.height, imageCSS)
  val beatmeterWaveStraightURI = conf.imageDirectory.map(_.resolve("waveStraight.svg")).getOrElse(getClass.getResource("/meter/waveform/waveStraight.svg").toURI)
  val beatmeterWaveStartURI = conf.imageDirectory.map(_.resolve("waveStart.svg")).getOrElse(getClass.getResource("/meter/waveform/waveStart.svg").toURI)
  val beatmeterWaveMiddleURI = conf.imageDirectory.map(_.resolve("waveMiddle.svg")).getOrElse(getClass.getResource("/meter/waveform/waveMiddle.svg").toURI)
  val beatmeterWaveEndURI = conf.imageDirectory.map(_.resolve("waveEnd.svg")).getOrElse(getClass.getResource("/meter/waveform/waveEnd.svg").toURI)
  val beatmeterWavePattern = (
    getImage(beatmeterWaveStraightURI, conf.height, imageCSS),
    getImage(beatmeterWaveStartURI, conf.height, imageCSS), IndexedSeq((
    getImage(beatmeterWaveMiddleURI, conf.height, imageCSS),
    getImage(beatmeterWaveEndURI, conf.height, imageCSS))))
  val beatmeterBeatHighlighted = getImage(beatmeterBeatURI, conf.height, imageHighlightedCSS)
  val beatmeterWaveHighlightedPattern = (
    getImage(beatmeterWaveStraightURI, conf.height, imageHighlightedCSS),
    getImage(beatmeterWaveStartURI, conf.height, imageHighlightedCSS), IndexedSeq((
    getImage(beatmeterWaveMiddleURI, conf.height, imageHighlightedCSS),
    getImage(beatmeterWaveEndURI, conf.height, imageHighlightedCSS))))

  override val minimalBeatDistance: Double = beatmeterBeat.getWidth / conf.speed / conf.width

  override def getElementStreams(beats: Seq[(Double, Boolean)], messages: Seq[(Double, Double, String)], frameCount: Int): List[TimedStream] = {
    val beatmeterStream = {
      def line(x: Double, w: Double, highlight: Boolean) = getImageLine(w.ceil.toInt, if (highlight) beatmeterWaveHighlightedPattern else beatmeterWavePattern).toPositioned(x, beatmeterY)

      val bs = beats.map(b => ((b._1 * conf.speed + conf.position) * conf.width - beatmeterBeat.getWidth / 2, b._2))

      val start = line(endPos * conf.width, (beats.head._1 * conf.speed + conf.position - endPos) * conf.width - beatmeterBeat.getWidth / 2.0, false)
      val middle = bs.sliding(2).map {
        case Seq((beat1, highlight1), (beat2, highlight2)) =>
          ElementStream(Positioned((beat1, beatmeterY), if (highlight1) beatmeterBeatHighlighted else beatmeterBeat)) ++
            line(beat1 + beatmeterBeat.getWidth, beat2 - beat1 - beatmeterBeat.getWidth, highlight1 && highlight2)
      }.reduceLeft(_ ++ _)
      val end = ElementStream(Positioned((bs.last._1, beatmeterY), if (bs.last._2) beatmeterBeatHighlighted else beatmeterBeat)) ++
        line(bs.last._1 + beatmeterBeat.getWidth, (frameCount / conf.frames) * conf.speed * conf.width - bs.last._1 + conf.startPosition * conf.width, false)

      start ++ middle ++ end
    }

    val messageFont = new Font(conf.messageFont._1,
      (if (conf.messageFont._3) {
        Font.BOLD
      } else {
        Font.PLAIN
      }) | (if (conf.messageFont._4) {
        Font.ITALIC
      } else {
        Font.PLAIN
      }),
      conf.messageFont._2)
    val messageStream = ElementStream(messages.toStream.map(b => (b._1 * conf.frames, b._2 * conf.frames, b._3)).map { b =>
      val startFrame = b._1.ceil.toInt
      val endFrame = b._2.ceil.toInt
      Timed(startFrame, endFrame, PositionDrawable(_ => (conf.messagePosition * conf.width, 0),
        (frame, g) => {
          val fadein = (frame.toDouble / conf.frames / 0.25).min(1.0)
          val fadeout = ((b._2 - b._1 - frame) / conf.frames / 0.25).min(1.0).max(0.0)
          val fade = fadein.min(fadeout)
          g.setFont(messageFont)
          g.setColor(toAWTColor(conf.messageColor, fade))
          val gv = messageFont.createGlyphVector(g.getFontRenderContext, b._3)
          val bounds = gv.getLogicalBounds
          val align = conf.messageAlign match {
            case AlignLeft => 0.0
            case AlignCenter => -bounds.getWidth / 2
            case AlignRight => -bounds.getWidth
          }
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
            RenderingHints.VALUE_ANTIALIAS_ON);
          val outline = gv.getOutline(align.toFloat, -bounds.getY.toFloat)
          g.fill(outline)
          g.setColor(toAWTColor(conf.messageBorderColor, fade))
          g.setStroke(new BasicStroke(conf.messageBorderStrength.toFloat))
          g.draw(outline)
        }
      ))
    })

    List(
      ElementStream(
        Stream(
          Fixed((endPos * conf.width, beatmeterY), beatmeterBackground)
        )
      ).toTimed().clip(new geom.Rectangle2D.Double(endPos * conf.width, beatmeterY, beatmeterWidth, conf.height)),
      beatmeterStream.toTimed(conf.speed * conf.width / conf.frames, conf.width),
        //.clip(new geom.Rectangle2D.Double(conf.width * endPos, beatmeterY, beatmeterWidth, conf.height)),
      ElementStream(
        Stream(
          Fixed((endPos * conf.width, beatmeterY), beatmeterForeground)
        )
      ).toTimed().clip(new geom.Rectangle2D.Double(endPos * conf.width, beatmeterY, beatmeterWidth, conf.height)),
      ElementStream(
        Stream(
          Fixed((conf.position * conf.width - beatmeterMarker.getWidth / 2.0, beatmeterY), beatmeterMarker),
          Fixed((startPos * conf.width - beatmeterStart.getWidth / 2.0, beatmeterY), beatmeterStart),
          Fixed((endPos * conf.width - beatmeterEnd.getWidth / 2.0, beatmeterY), beatmeterEnd)
        )
      ).toTimed(),
      messageStream
    )
  }

}
