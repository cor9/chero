package io.gitlab.sklavedaniel.beatmetergenerator

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import java.net.URI
import javax.imageio.ImageIO

import org.apache.batik.anim.dom.SVGDOMImplementation
import org.apache.batik.transcoder._
import org.apache.batik.transcoder.image.ImageTranscoder
import org.apache.batik.util.SVGConstants

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
    val bgColor = opt[Int](default = Some(0xB4FFFFFF), descr = "Color of the beatmeter background in ARGB")
    val foregroundImg = opt[File](descr = "Foreground svg image clipped to beatmeter width").map(_.toURI)
      .orElse(Some(getClass.getResource("/meter/foreground.svg").toURI))
    val startImg = opt[File](descr = "Left decoration svg image").map(_.toURI)
      .orElse(Some(getClass.getResource("/meter/start.svg").toURI))
    val endImg = opt[File](descr = "End decoration svg image").map(_.toURI)
      .orElse(Some(getClass.getResource("/meter/end.svg").toURI))
    val markerImg = opt[File](descr = "Target marker decoration svg image").map(_.toURI)
      .orElse(Some(getClass.getResource("/meter/marker.svg").toURI))
    val beatImg = opt[File](descr = "Beat svg image").map(_.toURI)
      .orElse(Some(getClass.getResource("/meter/beat.svg").toURI))
    val waveStraightImg = opt[File](descr = "Straight wave svg image").map(_.toURI)
      .orElse(Some(getClass.getResource("/meter/waveStraight.svg").toURI))
    val waveStartImg = opt[File](descr = "Start wave svg image").map(_.toURI)
      .orElse(Some(getClass.getResource("/meter/waveStart.svg").toURI))
    val waveEndImg = opt[File](descr = "End wave svg image").map(_.toURI)
      .orElse(Some(getClass.getResource("/meter/waveEnd.svg").toURI))
    val waveMiddleImg = opt[File](descr = "Middle wave svg image").map(_.toURI)
      .orElse(Some(getClass.getResource("/meter/waveMiddle.svg").toURI))

    override def execute(args: Array[String]) = new VideoGenerator(this).main(args)
  }

}

class VideoGenerator(conf: VideoGenerator.Conf) extends App {

  println(s"Estimated heights: video: ${conf.height}, beatmeter: ${conf.bmHeight}, foreground: ${conf.bmHeight}, background: ${conf.bmHeight}")

  val beatmeterForeground = getImage(conf.foregroundImg(), conf.fgHeight())
  val beatmeterMarker = getImage(conf.markerImg(), conf.fgHeight())
  val beatmeterStart = getImage(conf.startImg(), conf.fgHeight())
  val beatmeterEnd = getImage(conf.endImg(), conf.fgHeight())
  val beatmeterBeat = getImage(conf.beatImg(), conf.bmHeight())
  val beatmeterWaveStart = getImage(conf.waveStartImg(), conf.bmHeight())
  val beatmeterWaveMiddle = getImage(conf.waveMiddleImg(), conf.bmHeight())
  val beatmeterWaveEnd = getImage(conf.waveEndImg(), conf.bmHeight())
  val beatmeterWaveStraight = getImage(conf.waveStraightImg(), conf.bmHeight())
  val beatmeterWavePattern = (beatmeterWaveStraight, beatmeterWaveStart,
    IndexedSeq((beatmeterWaveMiddle, beatmeterWaveEnd)))


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
    for((beat1, beat2, _) <- beatDistances) {
      println(s"    $beat1 $beat2")
    }
    println("Increase beatmeter speed or beat distance")
    System.exit(1)
  }

  val imageSequence =
    getImageLine(((beats.head * conf.speed() + conf.target() - conf.end()) * conf.width() - beatmeterBeat.getWidth / 2.0).round.toInt, beatmeterWavePattern) ++
      beats.sliding(2).flatMap {
        case Seq(beat1, beat2) =>
          assert((beat2 - beat1) * conf.speed() * conf.width() - beatmeterBeat.getWidth >= 0, s"Beat distance to low. Maybe increase speed bettwen beats at $beat1 and $beat2.")
          beatmeterBeat +: getImageLine(((beat2 - beat1) * conf.speed() * conf.width() - beatmeterBeat.getWidth).round.toInt, beatmeterWavePattern)
      }.toStream ++
      Stream(beatmeterBeat) ++ getImageLine((conf.duration() * conf.width()).round.toInt, beatmeterWavePattern)
  val imagePositions = imageSequence.scanLeft((conf.end() * conf.width()).round.toInt) {
    case (pos, img) => pos + img.getWidth
  }
  var remainingImages = imageSequence.zip(imagePositions)

  conf.output().mkdirs()
  for (i <- 0 until frameCount) {
    println(s"Encoding frame ${i + 1} of $frameCount")
    val currentTime = i.toDouble / conf.frames()
    val image = new BufferedImage(conf.width(), conf.height(), BufferedImage.TYPE_INT_ARGB)
    val g = image.getGraphics

    g.setColor(new Color(conf.bgColor(), true))
    g.fillRect((conf.width() * conf.end()).round.toInt, beatmeterBackgroundY, (conf.width() * conf.start()).round.toInt - (conf.width() * conf.end()).round.toInt, conf.bgHeight())
    g.setClip((conf.width() * conf.end()).round.toInt, beatmeterY, (conf.width() * conf.start()).round.toInt - (conf.width() * conf.end()).round.toInt, conf.bmHeight())
    val currentOffset = (currentTime * conf.speed() * conf.width()).round.toInt
    remainingImages = remainingImages.dropWhile {
      case (img, pos) => pos - currentOffset + img.getWidth < 0
    }
    val currentImages = remainingImages.takeWhile {
      case (_, pos) => pos - currentOffset <= conf.width()
    }.map(x => (x._1, x._2 - currentOffset))
    for ((img, pos) <- currentImages) {
      g.drawImage(img, pos, beatmeterY, null)
    }
    g.setClip(null)

    g.drawImage(beatmeterMarker, (conf.target() * conf.width() - beatmeterMarker.getWidth / 2.0).round.toInt, beatmeterForegroundY, null)
    g.drawImage(clipImage(beatmeterForeground, beatmeterWidth).get, (conf.end() * conf.width()).round.toInt, beatmeterForegroundY, null)
    g.drawImage(beatmeterStart, (conf.start() * conf.width() - beatmeterStart.getWidth / 2.0).round.toInt, beatmeterForegroundY, null)
    g.drawImage(beatmeterEnd, (conf.end() * conf.width() - beatmeterEnd.getWidth / 2.0).round.toInt, beatmeterForegroundY, null)

    ImageIO.write(image, "PNG", new File(conf.output(), f"frame-$i%010d.png"))
  }

  def getImageLine(width: Int, pattern: (BufferedImage, BufferedImage, IndexedSeq[(BufferedImage, BufferedImage)])): Stream[BufferedImage] = {
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
      clipImage(pattern._1, width).toStream
    } else {
      val (endIdx, _, count) = counts.minBy(_._2)
      val imgs: Seq[BufferedImage] = for {
        i <- 0 until count
        img <- pattern._3
      } yield img._1
      val endImgs = (pattern._3.slice(0, endIdx).map(x => x._1) :+ pattern._3(endIdx)._2).toStream
      val restWidth = width - patternWidth * count - endImgs.map(_.getWidth).sum - pattern._2.getWidth
      assert(restWidth >= 0, s"Expected >= 0 but was $restWidth")
      val extraImgStart = clipImage(beatmeterWaveStraight, restWidth / 2)
      val extraImgEnd = clipImage(beatmeterWaveStraight, restWidth - restWidth / 2)
      extraImgStart.toStream ++ Stream(pattern._2) ++ imgs ++ endImgs ++ extraImgEnd.toStream
    }
    // would break laziness. assert(result.map(_.getWidth).sum - width == 0, s"Expected $width but was ${result.map(_.getWidth).sum}")
    result
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

    val input = new TranscoderInput(uri.toURL.openStream())

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
}
