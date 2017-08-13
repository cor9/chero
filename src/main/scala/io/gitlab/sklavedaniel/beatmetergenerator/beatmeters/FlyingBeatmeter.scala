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
import io.gitlab.sklavedaniel.beatmetergenerator.utils.{Align, AlignCenter, AlignLeft, AlignRight}

import scalafx.scene.paint.Color

object FlyingBeatmeter {

  case class Conf_V0_2_0(width: Int, height: Int, frames: Double, speed: Double, position: Double, beatColor: Color,
    beatBorderColor: Color, beatHighlightedColor: Color, beatHighlightedBorderColor: Color,
    messageFont: (String, Int, Boolean, Boolean), margin: Int, messageColor: Color, messageBorderColor: Color,
    messageBorderStrength: Double, messageAlign: Align, messagePosition: Double, imageDirectory: Option[URI]
  )

}

class FlyingBeatmeter(conf: FlyingBeatmeter.Conf_V0_2_0) extends Beatmeter2 {

  def width: Int = conf.width

  def frames: Double = conf.frames

  override val height = conf.height + conf.margin + conf.messageFont._2
  val beatmeterWidth = (1.0 - conf.position) * conf.width
  val beatmeterY = conf.messageFont._2 + conf.margin

  val imageCSS =
    s"""
        .wave.fill {
          fill: ${getCSSColor(conf.beatColor)} !important;
          fill-opacity: ${getCSSOpacity(conf.beatColor)} !important;
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

  val beatmeterBeatURI = conf.imageDirectory.map(_.resolve("beat.svg")).getOrElse(getClass.getResource("/meter/flying/beat.svg").toURI)
  val beatmeterBeat = getImage(beatmeterBeatURI, conf.height, imageCSS)
  val beatmeterBeatHighlighted = getImage(beatmeterBeatURI, conf.height, imageCSSHighlighted)
  val beatmeterAnimURI = conf.imageDirectory.map(_.resolve("beat.anim")).getOrElse(getClass.getResource("/meter/flying/beat.anim").toURI)
  val beatmeterAnim = getAnim(beatmeterAnimURI, conf.height, conf.frames, imageCSS)
  val beatmeterAnimHighlighted = getAnim(beatmeterAnimURI, conf.height, conf.frames, imageCSSHighlighted)

  override val minimalBeatDistance: Double = beatmeterBeat.getWidth / conf.speed / conf.width

  override def getElementStreams(beats: Seq[(Double, Boolean)], messages: Seq[(Double, Double, String)], frameCount: Int): List[TimedStream] = {
    val beatmeterStream = {
      val bs = beats.map(b => ((b._1 * conf.speed + conf.position) * conf.width - beatmeterBeat.getWidth / 2, b._2))

      val middle = bs.sliding(2).map {
        case Seq(beat1, beat2) =>
          ElementStream(Positioned((beat1._1, beatmeterY), if (beat1._2) {
            beatmeterBeatHighlighted
          } else {
            beatmeterBeat
          }))
      }.reduceLeft(_ ++ _)
      val end = ElementStream(Positioned((bs.last._1, beatmeterY), if (bs.last._2) {
        beatmeterBeatHighlighted
      } else {
        beatmeterBeat
      }))

      middle ++ end
    }
    val beatAnimStream = ElementStream(beats.toStream.map(b => (b._1 * conf.frames, b._2)).map { b =>
      val frame = b._1.ceil.toInt
      val anim = if (b._2) {
        beatmeterAnimHighlighted
      } else {
        beatmeterAnim
      }
      Timed(frame, frame + anim._2.size, PositionDrawable(_ => (conf.position * conf.width - anim._2.head.getWidth / 2, beatmeterY),
        AnimDrawable(frame - b._1, anim)))
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
      beatmeterStream.toTimed(conf.speed * conf.width / conf.frames, conf.position * conf.width, beatmeterWidth),
      beatAnimStream, messageStream
    )
  }

}
