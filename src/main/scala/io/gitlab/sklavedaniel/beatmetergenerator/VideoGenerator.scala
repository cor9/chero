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
    val height = opt[Int](default = Some(30), descr = "Width of generated video")
    val speed = opt[Double](default = Some(0.2), descr = "Speed of the beatmeter in video widths per second")
    val start = opt[Double](default = Some(0.95), descr = "Left position of the beatmeter relative to width (Between 0 and 1)")
    val end = opt[Double](default = Some(0.05), descr = "Right position of the beatmeter relative to width (Between 0 and 1)")
    val target = opt[Double](default = Some(0.2), descr = "Target position of the beatmeter relative to width (Between 0 and 1)")
    val bmHeight = opt[Int](default = Some(20), descr = "Height of the actual beatmeter")
    val fgHeight = opt[Int](default = Some(28), descr = "Height of the forground decorations")
    val bgHeight = opt[Int](default = Some(20), descr = "Height of the beatmeter background")
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

class VideoGenerator(Conf: VideoGenerator.Conf) extends App {

  val beatmeterForeground = getImage(Conf.foregroundImg(), Conf.fgHeight())
  val beatmeterMarker = getImage(Conf.markerImg(), Conf.fgHeight())
  val beatmeterStart = getImage(Conf.startImg(), Conf.fgHeight())
  val beatmeterEnd = getImage(Conf.endImg(), Conf.fgHeight())
  val beatmeterBeat = getImage(Conf.beatImg(), Conf.bmHeight())
  val beatmeterWaveStart = getImage(Conf.waveStartImg(), Conf.bmHeight())
  val beatmeterWaveMiddle = getImage(Conf.waveMiddleImg(), Conf.bmHeight())
  val beatmeterWaveEnd = getImage(Conf.waveEndImg(), Conf.bmHeight())
  val beatmeterWaveStraight = getImage(Conf.waveStraightImg(), Conf.bmHeight())
  val beatmeterWavePattern = (beatmeterWaveStraight, beatmeterWaveStart,
    IndexedSeq((beatmeterWaveMiddle, beatmeterWaveEnd)))


  val beats: Seq[Double] = BeatFiles.load(Conf.input()).map(_ / 1000.0)

  val beatmeterMiddle = Conf.height() / 2.0
  val beatmeterWidth = ((Conf.start() - Conf.end()) * Conf.width()).round.toInt
  val beatmeterY = (beatmeterMiddle - Conf.bmHeight() / 2.0).round.toInt
  val beatmeterBackgroundY = (beatmeterMiddle - Conf.bgHeight() / 2.0).round.toInt
  val beatmeterForegroundY = (beatmeterMiddle - Conf.fgHeight() / 2.0).round.toInt
  val frameCount = (Conf.frames() * Conf.duration()).round.toInt

  def currentPosition(targetTime: Double, currentTime: Double): Double =
    (targetTime - currentTime) * Conf.speed() + Conf.target()

  assert(beats.size > 2, "You need at least two beats.")

  val imageSequence =
    getImageLine(((beats.head * Conf.speed() + Conf.target() - Conf.end()) * Conf.width() - beatmeterBeat.getWidth / 2.0).round.toInt, beatmeterWavePattern) ++
      beats.sliding(2).filter(_.size == 2).flatMap {
        case Seq(beat1, beat2) =>
          assert((beat2 - beat1) * Conf.speed() * Conf.width() - beatmeterBeat.getWidth >= 0, s"Beat distance to low. Maybe increase speed bettwen beats at $beat1 and $beat2.")
          beatmeterBeat +: getImageLine(((beat2 - beat1) * Conf.speed() * Conf.width() - beatmeterBeat.getWidth).round.toInt, beatmeterWavePattern)
      }.toStream ++
      Stream(beatmeterBeat) ++ getImageLine((Conf.duration() * Conf.width()).round.toInt, beatmeterWavePattern)
  val imagePositions = imageSequence.scanLeft((Conf.end() * Conf.width()).round.toInt) {
    case (pos, img) => pos + img.getWidth
  }
  var remainingImages = imageSequence.zip(imagePositions)

  Conf.output().mkdirs()
  for (i <- 0 until frameCount) {
    println(s"Encoding frame ${i + 1} of $frameCount")
    val currentTime = i.toDouble / Conf.frames()
    val image = new BufferedImage(Conf.width(), Conf.height(), BufferedImage.TYPE_INT_ARGB)
    val g = image.getGraphics

    g.setColor(new Color(Conf.bgColor()))
    g.fillRect((Conf.width() * Conf.end()).round.toInt, beatmeterBackgroundY, (Conf.width() * Conf.start()).round.toInt - (Conf.width() * Conf.end()).round.toInt, Conf.bgHeight())
    g.setClip((Conf.width() * Conf.end()).round.toInt, beatmeterY, (Conf.width() * Conf.start()).round.toInt - (Conf.width() * Conf.end()).round.toInt, Conf.bmHeight())
    val currentOffset = (currentTime * Conf.speed() * Conf.width()).round.toInt
    remainingImages = remainingImages.dropWhile {
      case (img, pos) => pos - currentOffset + img.getWidth < 0
    }
    val currentImages = remainingImages.takeWhile {
      case (_, pos) => pos - currentOffset <= Conf.width()
    }.map(x => (x._1, x._2 - currentOffset))
    for ((img, pos) <- currentImages) {
      g.drawImage(img, pos, beatmeterY, null)
    }
    g.setClip(null)

    g.drawImage(beatmeterMarker, (Conf.target() * Conf.width() - beatmeterMarker.getWidth / 2.0).round.toInt, beatmeterForegroundY, null)
    g.drawImage(clipImage(beatmeterForeground, beatmeterWidth).get, (Conf.end() * Conf.width()).round.toInt, beatmeterForegroundY, null)
    g.drawImage(beatmeterStart, (Conf.start() * Conf.width() - beatmeterStart.getWidth / 2.0).round.toInt, beatmeterForegroundY, null)
    g.drawImage(beatmeterEnd, (Conf.end() * Conf.width() - beatmeterEnd.getWidth / 2.0).round.toInt, beatmeterForegroundY, null)

    ImageIO.write(image, "PNG", new File(Conf.output(), f"frame-$i%010d.png"))
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
