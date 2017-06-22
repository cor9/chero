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

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import java.net.URI
import javax.imageio.ImageIO

import org.apache.batik.anim.dom.{SAXSVGDocumentFactory, SVGDOMImplementation}
import org.apache.batik.transcoder._
import org.apache.batik.transcoder.image.ImageTranscoder
import org.apache.batik.util.{SVGConstants, XMLResourceDescriptor}
import org.rogach.scallop.ScallopOption
import shapeless.{HNil, :: => :::}

object VideoGenerator {

  class Conf extends Main.ExecutableSubcommand("video") {
    val input = opt[File](required = true, descr = "File containing beat definitions")
    validateFileExists(input)
    val output = opt[File](required = true, descr = "Directory for generated images")
    validateFileDoesNotExist(output)
    val duration = opt[Double](required = true, descr = "Duration of generated image sequence in seconds")
    val frames = opt[Double](default = Some(25), descr = "Frames per second")
    val width = opt[Int](required = true, descr = "Width of generated video")
    val height = opt[Int](descr = "Width of generated video").orElse(width.toOption.map(x => (x / 42.66).round.toInt))
    val speed = opt[Double](default = Some(0.2), descr = "Speed of the beatmeter in video widths per second")
    val start = opt[Double](default = Some(0.95), descr = "Left position of the beatmeter relative to width (Between 0 and 1)")
    val end = opt[Double](default = Some(0.05), descr = "Right position of the beatmeter relative to width (Between 0 and 1)")
    val target = opt[Double](default = Some(0.2), descr = "Target position of the beatmeter relative to width (Between 0 and 1)")
    val bmHeight = opt[Int](descr = "Height of the actual beatmeter").orElse(height.toOption.map(x => (x * 2 / 3.0).round.toInt))
    val fgHeight = opt[Int](descr = "Height of the forground decorations").orElse(height.toOption.map(x => (x * 14 / 15.0).round.toInt))
    val bgHeight = opt[Int](descr = "Height of the beatmeter background").orElse(bmHeight.toOption)
    val bgColor = opt[Color](default = Some(new Color(0xbbaaaaaa, true)), descr = "Color of the beatmeter background in ARGB")
    val fgColor = opt[Color](default = Some(new Color(0xbb4d4d4d, true)), descr = "Color of the foreground decorations ARGB")
    val wvColor = opt[Color](default = Some(new Color(0xffff0980, true)), descr = "Color of the wave in ARGB")
    val mrColor = opt[Color](default = Some(new Color(0xbb4d4d4d, true)), descr = "Color of the marker in ARGB")
    val foregroundImg = opt[Option[File]](descr = "Foreground svg image clipped to beatmeter width").map(_.map(_.toURI))
      .orElse(Some(Some(getClass.getResource("/meter/foreground.svg").toURI)))
    val startImg = opt[Option[File]](descr = "Left decoration svg image").map(_.map(_.toURI))
      .orElse(Some(Some(getClass.getResource("/meter/start.svg").toURI)))
    val endImg = opt[Option[File]](descr = "End decoration svg image").map(_.map(_.toURI))
      .orElse(Some(Some(getClass.getResource("/meter/end.svg").toURI)))
    val markerImg = opt[Option[File]](descr = "Target marker decoration svg image").map(_.map(_.toURI))
      .orElse(Some(Some(getClass.getResource("/meter/marker.svg").toURI)))
    val beatImg = opt[File](descr = "Beat svg image").map(_.toURI)
      .orElse(Some(getClass.getResource("/meter/beat.svg").toURI))
    val wavePattern = opt[Option[File ::: File ::: File ::: File ::: HNil]](descr = "Images for the wave line between beets given as --wave-pattern straightLine.svg waveStart.svg waveMiddle.Svg waveEnd.svg")
      .map(_.map(t => t.head.toURI :: t.tail.head.toURI :: t.tail.tail.head.toURI :: t.tail.tail.tail.head.toURI :: HNil))
      .orElse(Some(Some(getClass.getResource("/meter/waveStraight.svg").toURI ::
        getClass.getResource("/meter/waveStart.svg").toURI ::
        getClass.getResource("/meter/waveEnd.svg").toURI ::
        getClass.getResource("/meter/waveMiddle.svg").toURI :: HNil
      )))

    override def execute(args: Array[String]) = new VideoGenerator(this).main(args)
  }

}

class VideoGenerator(conf: VideoGenerator.Conf) extends App {

  println(s"Heights: video: ${conf.height()}, beatmeter: ${conf.bmHeight()}, foreground: ${conf.bmHeight()}, background: ${conf.bmHeight()}")

  val emptyWave = new BufferedImage(conf.bmHeight(), 1000, BufferedImage.TYPE_INT_ARGB)
  val beatmeterForeground = conf.foregroundImg().map(getImage(_, conf.fgHeight()))
  val beatmeterMarker = conf.markerImg().map(getImage(_, conf.fgHeight()))
  val beatmeterStart = conf.startImg().map(getImage(_, conf.fgHeight()))
  val beatmeterEnd = conf.endImg().map(getImage(_, conf.fgHeight()))
  val beatmeterBeat = getImage(conf.beatImg(), conf.bmHeight())
  val beatmeterWavePattern = conf.wavePattern().map(t => (
    getImage(t.head, conf.bmHeight()),
      getImage(t.tail.head, conf.bmHeight()), IndexedSeq((
        getImage(t.tail.tail.head, conf.bmHeight()),
          getImage(t.tail.tail.tail.head, conf.bmHeight())))))


  val beats: Seq[Double] = BeatFiles.load(conf.input())
  val beatmeterMiddle = conf.height() / 2.0
  val beatmeterWidth = ((conf.start() - conf.end()) * conf.width()).round.toInt
  val beatmeterY = (beatmeterMiddle - conf.bmHeight() / 2.0).round.toInt
  val beatmeterBackgroundY = (beatmeterMiddle - conf.bgHeight() / 2.0).round.toInt
  val beatmeterForegroundY = (beatmeterMiddle - conf.fgHeight() / 2.0).round.toInt
  val frameCount = (conf.frames() * conf.duration()).round.toInt

  def currentPosition(targetTime: Double, currentTime: Double): Double =
    (targetTime - currentTime) * conf.speed() + conf.target()

  assert(beats.size > 2, "You need at least two beats.")

  val beatDistances = beats.sliding(2).flatMap {
    case Seq(beat1, beat2) =>
      Some((beat1, beat2, (beat2 - beat1) * conf.speed() * conf.width())).filter(_._3 < beatmeterBeat.getWidth)
  }.toStream
  if (!beatDistances.isEmpty) {
    println(s"Error: The following beats have distance below ${beatmeterBeat.getWidth / conf.speed() / conf.width()}:")
    for ((beat1, beat2, _) <- beatDistances) {
      println(s"    $beat1 $beat2")
    }
    println("Increase beatmeter speed or beat distance")
    System.exit(1)
  }

  val imageSequence =
    getImageLine(((beats.head * conf.speed() + conf.target() - conf.end()) * conf.width() - beatmeterBeat.getWidth / 2.0).round.toInt,
      beatmeterWavePattern) ++
      beats.map(b => (b * conf.speed() * conf.width()).round.toInt).sliding(2).flatMap {
        case Seq(beat1, beat2) =>
          assert((beat2 - beat1) - beatmeterBeat.getWidth >= 0, s"Beat distance to low. Maybe increase speed bettwen beats at $beat1 and $beat2.")
          Left(beatmeterBeat) +: getImageLine((beat2 - beat1 - beatmeterBeat.getWidth), beatmeterWavePattern)
      }.toStream ++
      Stream(Left(beatmeterBeat)) ++ getImageLine((conf.duration() * conf.width()).round.toInt, beatmeterWavePattern)
  val imagePositions = imageSequence.scanLeft((conf.end() * conf.width()).round.toInt) {
    case (pos, Left(img)) => pos + img.getWidth
    case (pos, Right(delta)) => pos + delta
  }
  var remainingImages = imageSequence.zip(imagePositions)

  conf.output().mkdirs()
  for (i <- 0 until frameCount) {
    println(s"Encoding frame ${i + 1} of $frameCount")
    val currentTime = i.toDouble / conf.frames()
    val image = new BufferedImage(conf.width(), conf.height(), BufferedImage.TYPE_INT_ARGB)
    val g = image.getGraphics

    g.setColor(conf.bgColor())
    g.fillRect((conf.width() * conf.end()).round.toInt, beatmeterBackgroundY, (conf.width() * conf.start()).round.toInt - (conf.width() * conf.end()).round.toInt, conf.bgHeight())
    g.setClip((conf.width() * conf.end()).round.toInt, beatmeterY, (conf.width() * conf.start()).round.toInt - (conf.width() * conf.end()).round.toInt, conf.bmHeight())
    val currentOffset = (currentTime * conf.speed() * conf.width()).round.toInt
    remainingImages = remainingImages.dropWhile {
      case (Left(img), pos) => pos - currentOffset + img.getWidth < 0
      case (Right(delta), pos) => pos - currentOffset + delta < 0
    }
    val currentImages = remainingImages.takeWhile {
      case (_, pos) => pos - currentOffset <= conf.width()
    }.map(x => (x._1, x._2 - currentOffset))
    for ((tmp, pos) <- currentImages; img <- tmp.left) {
      g.drawImage(img, pos, beatmeterY, null)
    }
    g.setClip(null)

    beatmeterMarker.foreach(img =>
      g.drawImage(img, (conf.target() * conf.width() - img.getWidth / 2.0).round.toInt, beatmeterForegroundY, null)
    )
    beatmeterForeground.foreach(img =>
      g.drawImage(clipImage(img, beatmeterWidth).get, (conf.end() * conf.width()).round.toInt, beatmeterForegroundY, null)
    )
    beatmeterStart.foreach(img =>
      g.drawImage(img, (conf.start() * conf.width() - img.getWidth / 2.0).round.toInt, beatmeterForegroundY, null))
    beatmeterEnd.foreach(img =>
      g.drawImage(img, (conf.end() * conf.width() - img.getWidth / 2.0).round.toInt, beatmeterForegroundY, null)
    )

    ImageIO.write(image, "PNG", new File(conf.output(), f"frame-$i%010d.png"))
  }

  println(s"Heights: video: ${conf.height()}, beatmeter: ${conf.bmHeight()}, foreground: ${conf.bmHeight()}, background: ${conf.bmHeight()}")

  def getImageLine(width: Int, pattern: Option[(BufferedImage, BufferedImage, IndexedSeq[(BufferedImage, BufferedImage)])]): Stream[Either[BufferedImage, Int]] = {
    pattern match {
      case Some(pattern) =>
        val patternWidth = pattern._3.map(_._1.getWidth).sum
        val counts = for {
          i <- pattern._3.indices
          endWidth = pattern._3.slice(0, i).map(_._1.getWidth).sum + pattern._3(i)._2.getWidth + pattern._2.getWidth
          if endWidth <= width
        } yield {
          val tmp = width - endWidth
          (i, tmp % patternWidth, tmp / patternWidth)
        }
        val result = if (counts.isEmpty) {
          clipImage(pattern._1, width).map(Left(_)).toStream
        } else {
          val (endIdx, _, count) = counts.minBy(_._2)
          val imgs: Seq[Either[BufferedImage, Int]] = for {
            _ <- 0 until count
            img <- pattern._3
          } yield Left(img._1)
          val endImgs = (pattern._3.slice(0, endIdx).map(x => x._1) :+ pattern._3(endIdx)._2).toStream
          val restWidth = width - patternWidth * count - endImgs.map(_.getWidth).sum - pattern._2.getWidth
          assert(restWidth >= 0, s"Expected >= 0 but was $restWidth")
          val extraImgStart = clipImage(pattern._1, restWidth / 2).map(Left(_))
          val extraImgEnd = clipImage(pattern._1, restWidth - restWidth / 2).map(Left(_))
          extraImgStart.toStream ++ Stream(Left(pattern._2)) ++ imgs ++ endImgs.map(Left(_)) ++ extraImgEnd.toStream
        }
        // would break laziness. assert(result.map(_.getWidth).sum - width == 0, s"Expected $width but was ${result.map(_.getWidth).sum}")
        result
      case None => Stream(Right(width))
    }
  }

  def clipImage(image: BufferedImage, width: Int): Option[BufferedImage] = if (width > 0) {
    val result = new BufferedImage(width, image.getHeight, image.getType)
    result.getGraphics.drawImage(image, 0, 0, null)
    Some(result)
  } else None

  def getImage(uri: URI, height: Float): BufferedImage = {
    val hints = new TranscodingHints()
    hints.put(SVGAbstractTranscoder.KEY_HEIGHT, height)
    hints.put(XMLAbstractTranscoder.KEY_XML_PARSER_VALIDATING, false)
    hints.put(XMLAbstractTranscoder.KEY_DOM_IMPLEMENTATION, SVGDOMImplementation.getDOMImplementation)
    hints.put(XMLAbstractTranscoder.KEY_DOCUMENT_ELEMENT_NAMESPACE_URI, SVGConstants.SVG_NAMESPACE_URI)
    hints.put(XMLAbstractTranscoder.KEY_DOCUMENT_ELEMENT, "svg")

    val docfactory = new SAXSVGDocumentFactory(XMLResourceDescriptor.getXMLParserClassName())
    val doc = docfactory.createDocument(uri.toString)
    val style = doc.createElementNS("http://www.w3.org/2000/svg", "style")

    val (wvA, wvRgb) = getColor(conf.wvColor())
    val (fgA, fgRgb) = getColor(conf.fgColor())
    val (mrA, mrRgb) = getColor(conf.mrColor())
    style.setTextContent(
      s"""
        .wave.fill {
          fill: #$wvRgb !important;
          fill-opacity: $wvA !important;
        }
        .wave.stroke {
          stroke: #$wvRgb !important;
          stroke-opacity: $wvA !important;
        }
        .foreground.fill {
          fill: #$fgRgb !important;
          fill-opacity: $fgA !important;
        }
        .foreground.stroke {
          stroke: #$fgRgb !important;
          stroke-opacity: $fgA !important;
        }
        .marker.fill {
          fill: #$mrRgb !important;
          fill-opacity: $mrA !important;
        }
        .marker.stroke {
          stroke: #$mrRgb !important;
          stroke-opacity: $mrA !important;
        }
      """)
    doc.getDocumentElement.appendChild(style)
    val input = new TranscoderInput(doc)


    var image = None: Option[BufferedImage]

    val rasterizer = new ImageTranscoder {
      override def writeImage(img: BufferedImage, output: TranscoderOutput): Unit = {
        image = Some(img)
      }

      override def createImage(width: Int, height: Int) =
        new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    }

    rasterizer.setTranscodingHints(hints)
    rasterizer.transcode(input, null)

    image.get
  }

  def getColor(color: Color) = {
    val rgb = (color.getRGB & 0xffffff).toHexString
    val a = color.getAlpha.toFloat / 0xff
    (a.toString, "0" * (6 - rgb.length) + rgb)
  }

}
