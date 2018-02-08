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

import java.awt.{BasicStroke, Font, RenderingHints}
import java.net.URI

import io.gitlab.sklavedaniel.beatmetergenerator.beatmeters.Beatmeter._
import io.gitlab.sklavedaniel.beatmetergenerator.utils._

import scalafx.scene.paint.Color

object FlyingBeatmeter {

  case class Conf_V0_2_0(width: Int, height: Int, frames: Double, speed: Double, position: Double, beatColor: Color,
    beatBorderColor: Color, beatHighlightedColor: Color, beatHighlightedBorderColor: Color,
    messageFont: (String, Int, Boolean, Boolean), margin: Int, messageColor: Color, messageBorderColor: Color,
    messageBorderStrength: Double, messageAlign: AlignH, messagePosition: Double, imageDirectory: Option[URI]
  ) {
    def toV0_2_3 = Conf_V0_2_3(width, height, frames, speed, position, beatColor, beatBorderColor, beatHighlightedColor,
      beatHighlightedBorderColor, 1.5, 0.2, Color.Transparent, Color.Transparent, messageFont, margin, messageColor,
      messageBorderColor, messageBorderStrength, messageAlign, messagePosition, imageDirectory)
  }

  case class Conf_V0_2_3(width: Int, height: Int, frames: Double, speed: Double, position: Double, beatColor: Color,
    beatBorderColor: Color, beatHighlightedColor: Color, beatHighlightedBorderColor: Color, beatScale: Double, beatDuration: Double,
    beatMarkerColor: Color, backgroundColor: Color, messageFont: (String, Int, Boolean, Boolean),
    margin: Int, messageColor: Color, messageBorderColor: Color,
    messageBorderStrength: Double, messageAlign: AlignH, messagePosition: Double, imageDirectory: Option[URI]
  )

}

class FlyingBeatmeter(conf: FlyingBeatmeter.Conf_V0_2_3) extends Beatmeter {

  def width: Int = conf.width

  def frames: Double = conf.frames

  override val height = (conf.height * conf.beatScale).ceil.toInt + conf.margin + conf.messageFont._2
  val beatmeterWidth = (1.0 - conf.position) * conf.width
  val beatmeterY = conf.messageFont._2 + conf.margin + conf.height * conf.beatScale / 2.0

  val imageCSS =
    s"""
        .wave.fill {
          fill: ${getCSSColor(conf.beatColor)} !important;
          fill-opacity: ${getCSSOpacity(conf.beatColor)} !important;
          width: 100px !important;
        }
        .wave.stroke {
          stroke: ${getCSSColor(conf.beatColor)} !important;
          stroke-opacity: ${getCSSOpacity(conf.beatColor)} !important;
        }
        .foreground.fill {
          fill: ${getCSSColor(conf.beatBorderColor)} !important;
          fill-opacity: ${getCSSOpacity(conf.beatBorderColor)} !important;
        }
        .foreground.stroke {
          stroke: ${getCSSColor(conf.beatBorderColor)} !important;
          stroke-opacity: ${getCSSOpacity(conf.beatBorderColor)} !important;
        }
        .background.fill {
          fill: ${getCSSColor(conf.backgroundColor)} !important;
          fill-opacity: ${getCSSOpacity(conf.backgroundColor)} !important;
        }
        .background.stroke {
          stroke: ${getCSSColor(conf.backgroundColor)} !important;
          stroke-opacity: ${getCSSOpacity(conf.backgroundColor)} !important;
        }
    """
  val imageCSSHighlighted =
    s"""
        .wave.fill {
          fill: ${getCSSColor(conf.beatHighlightedColor)} !important;
          fill-opacity: ${getCSSOpacity(conf.beatHighlightedColor)} !important;
        }
        .wave.stroke {
          stroke: ${getCSSColor(conf.beatHighlightedColor)} !important;
          stroke-opacity: ${getCSSOpacity(conf.beatHighlightedColor)} !important;
        }
        .foreground.fill {
          fill: ${getCSSColor(conf.beatHighlightedBorderColor)} !important;
          fill-opacity: ${getCSSOpacity(conf.beatHighlightedBorderColor)} !important;
        }
        .foreground.stroke {
          stroke: ${getCSSColor(conf.beatHighlightedBorderColor)} !important;
          stroke-opacity: ${getCSSOpacity(conf.beatHighlightedBorderColor)} !important;
        }
    """
  val imageCSSMarker =
    s"""
        .marker.fill {
          fill: ${getCSSColor(conf.beatMarkerColor)} !important;
          fill-opacity: ${getCSSOpacity(conf.beatMarkerColor)} !important;
        }
        .marker.stroke {
          stroke: ${getCSSColor(conf.beatMarkerColor)} !important;
          stroke-opacity: ${getCSSOpacity(conf.beatMarkerColor)} !important;
        }
    """

  val beatmeterBackgroundURI = conf.imageDirectory.map(_.resolve("background.svg")).getOrElse(getClass.getResource("/meter/waveform/background.svg").toURI)
  val beatmeterBackground = getImage(beatmeterBackgroundURI, (conf.height * conf.beatScale).toFloat, imageCSS)
  val beatmeterBeatURI = conf.imageDirectory.map(_.resolve("beat.svg")).getOrElse(getClass.getResource("/meter/flying/beat.svg").toURI)
  val beatmeterBeat = new SVGDrawable(beatmeterBeatURI, conf.height, imageCSS, AlignCenter, AlignMiddle)
  val beatmeterBeatHighlighted = new SVGDrawable(beatmeterBeatURI, conf.height, imageCSSHighlighted, AlignCenter, AlignMiddle)
  val beatmeterBeatAnimFrames = (conf.beatDuration * frames).ceil.toInt

  def delta(i: Int) = (conf.beatScale - 1.0) * Math.sin(Math.PI * i / beatmeterBeatAnimFrames.toDouble)

  val beatmeterBeatAnim = (0 until beatmeterBeatAnimFrames).map { i =>
    new SVGDrawable(beatmeterBeatURI, conf.height + conf.height * delta(i), imageCSS, AlignCenter, AlignMiddle)
  }
  val beatmeterBeatHighlightedAnim = (0 until beatmeterBeatAnimFrames).map { i =>
    new SVGDrawable(beatmeterBeatURI, (conf.height + conf.height * delta(i)).toFloat, imageCSSHighlighted, AlignCenter, AlignMiddle)
  }
  val beatmeterMarkerURI = conf.imageDirectory.map(_.resolve("marker.svg")).getOrElse(getClass.getResource("/meter/flying/marker.svg").toURI)
  val beatmeterMarker = new SVGDrawable(beatmeterMarkerURI, conf.height * conf.beatScale, imageCSSMarker, AlignCenter, AlignMiddle)

  override val minimalBeatDistance: Double = beatmeterBeat.width / conf.speed / conf.width

  override def getElementStreams(beats: Seq[(Double, Boolean)], messages: Seq[(Double, Double, String)], frameCount: Int): List[TimedStream] = {
    val beatmeterStream = {
      val bs = beats.map(b => ((b._1 * conf.speed + conf.position) * conf.width, b._2)).toStream
      ElementStream(bs.map {
        beat1 =>
          val img = if (beat1._2) {
            beatmeterBeatHighlighted
          } else {
            beatmeterBeat
          }
          Positioned((beat1._1, beatmeterY), 0.0, img)
      })
    }
    val beatAnimStream = ElementStream((beats :+ (Double.PositiveInfinity, true)).sliding(2).toStream.map(b => (b.head._1 * conf.frames, b.head._2, b.tail.head._1 * conf.frames)).map { b =>
      val frame = b._1.ceil.toInt
      val beat = if (b._2) {
        beatmeterBeatHighlightedAnim
      } else {
        beatmeterBeatAnim
      }
      Timed(frame, (frame + beat.size).min(b._3.round.toInt), PositionDrawable(_ => (conf.position * conf.width
        , beatmeterY),
        FramesDrawable(beat)))
    })


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
          val outline = gv.getOutline(align.toFloat, -bounds.getY.toFloat)
          g.fill(outline)
          g.setColor(toAWTColor(conf.messageBorderColor, fade))
          g.setStroke(new BasicStroke(conf.messageBorderStrength.toFloat))
          g.draw(outline)
        }
      ))
    })

    List(
      ElementStream(Stream(
        Fixed((0.0, beatmeterY), ImageDrawable(beatmeterBackground, AlignCenter, AlignMiddle))
      )).toTimed(),
      beatmeterStream.toTimed(conf.speed * conf.width / conf.frames, conf.position * conf.width, beatmeterWidth, 0.5),
      beatAnimStream, messageStream,
      ElementStream(Fixed((conf.position * conf.width, beatmeterY), beatmeterMarker)).toTimed()
    )
  }

}
