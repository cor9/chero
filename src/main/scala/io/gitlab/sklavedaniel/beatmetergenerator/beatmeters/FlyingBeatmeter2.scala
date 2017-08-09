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

import java.awt
import java.awt.{Font, RenderingHints}
import java.net.URI

import io.gitlab.sklavedaniel.beatmetergenerator.beatmeters.Beatmeter._

import scalafx.scene.paint.Color

object FlyingBeatmeter2 {

  case class Conf(width: Int, height: Int, frames: Double, speed: Double, position: Double, beatColor: Color,
    beatBorderColor: Color, beatHighlightedColor: Color, beatHighlightedBorderColor: Color, beatImage: Option[URI],
    beatAnimation: Option[URI], messageHeight: Int, margin: Int, messageColor: Color
  )

}

class FlyingBeatmeter2(conf: FlyingBeatmeter2.Conf) extends Beatmeter2 {

  def width: Int = conf.width

  def frames: Double = conf.frames

  override val height = conf.height + conf.margin + conf.messageHeight
  val beatmeterWidth = (1.0 - conf.position) * conf.width
  val beatmeterY = conf.messageHeight + conf.margin

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

  val beatmeterBeat = getImage(conf.beatImage.getOrElse(getClass.getResource("/meter/flying/beat.svg").toURI), conf.height, imageCSS)
  val beatmeterBeatHighlighted = getImage(conf.beatImage.getOrElse(getClass.getResource("/meter/flying/beat.svg").toURI), conf.height, imageCSSHighlighted)
  val beatmeterAnim = getAnim(conf.beatAnimation.getOrElse(getClass.getResource("/meter/flying/beat.anim").toURI), conf.height, conf.frames, imageCSS)
  val beatmeterAnimHighlighted = getAnim(conf.beatAnimation.getOrElse(getClass.getResource("/meter/flying/beat.anim").toURI), conf.height, conf.frames, imageCSSHighlighted)

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
    val messageStream = ElementStream(messages.toStream.map(b => (b._1 * conf.frames, b._2 * conf.frames, b._3)).map { b =>
      val startFrame = b._1.ceil.toInt
      val endFrame = b._2.ceil.toInt
      val font = new Font("Dialog", Font.PLAIN, conf.messageHeight)
      Timed(startFrame, endFrame, PositionDrawable(_ => (conf.width / 2, 0),
        (frame, g) => {
          val fadein = (frame.toDouble / conf.frames / 0.25).min(1.0)
          val fadeout = ((b._2 - b._1 - frame) / conf.frames / 0.25).min(1.0).max(0.0)
          val fade = fadein.min(fadeout)
          g.setFont(font)
          val color = new awt.Color(conf.messageColor.red.toFloat, conf.messageColor.green.toFloat,
            conf.messageColor.blue.toFloat, (conf.messageColor.opacity * fade).toFloat)
          g.setColor(color)
          g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
          val bounds = g.getFontMetrics.getStringBounds(b._3, g)
          g.drawString(b._3, -bounds.getWidth.toFloat / 2, -bounds.getY.toFloat)
        }
      ))
    })

    List(
      beatmeterStream.toTimed(conf.speed * conf.width / conf.frames, conf.position * conf.width, beatmeterWidth),
      beatAnimStream, messageStream
    )
  }

}
