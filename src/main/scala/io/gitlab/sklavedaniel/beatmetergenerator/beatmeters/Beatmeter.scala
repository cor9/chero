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
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.awt.{AlphaComposite, Color, Graphics2D, Shape}
import java.net.URI

import io.gitlab.sklavedaniel.beatmetergenerator.utils._
import org.apache.batik.anim.dom.{SAXSVGDocumentFactory, SVGDOMImplementation}
import org.apache.batik.bridge.{BridgeContext, DocumentLoader, GVTBuilder, UserAgentAdapter}
import org.apache.batik.gvt.GraphicsNode
import org.apache.batik.transcoder._
import org.apache.batik.transcoder.image.ImageTranscoder
import org.apache.batik.util.{SVGConstants, XMLResourceDescriptor}

import scala.io.Source
import scalafx.scene
import scalafx.scene.paint

object Beatmeter {

  def toAWTColor(color: scene.paint.Color, fade: Double = 1.0) = {
    new awt.Color(color.red.toFloat, color.green.toFloat,
      color.blue.toFloat, (fade * color.opacity).toFloat)
  }

  object ElementStream {
    def apply[B <: Element]() = new ElementStream(None, Stream[B]())

    def apply[B <: Element](stream: Stream[B]) = new ElementStream(None, stream)

    def apply[B <: Element](element: B) = new ElementStream(None, Stream(element))
  }

  case class ElementStream[+A <: Option[Shape], B <: Element](clip: A, stream: Stream[B]) {
    def toPositioned(offset: Double, y: Double)(implicit ev: B =:= Appended) =
      ElementStream(clip,
        if (stream.nonEmpty) {
          stream.tail.scanLeft(Positioned((offset, y), ev(stream.head).width, ev(stream.head).drawable)) {
            case (Positioned((pos, _), width, _), drawable) =>
              Positioned((pos + width, y), ev(drawable).width, ev(drawable).drawable)
          }
        } else {
          Stream()
        })

    def toTimed(pxPerFrame: Double, width: Double)(implicit ev: B =:= Positioned): ElementStream[A, Timed] = toTimed(pxPerFrame, 0.0, width)

    def toTimed(pxPerFrame: Double, offset: Double, width: Double, disappear: Double = 1.0)(implicit ev: B =:= Positioned): ElementStream[A, Timed] =
      ElementStream(clip,
        stream.map { case Positioned((x, y), w, drawable) =>
          val startFrame = (x - width - offset) / pxPerFrame
          val error = (startFrame.ceil - startFrame) * pxPerFrame
          Timed(startFrame.ceil.toInt,
            ((x + w * disappear - offset) / pxPerFrame).ceil.toInt,
            PositionDrawable(
              i => (width - i * pxPerFrame - error + offset, y),
              drawable))
        })

    def toTimed()(implicit ev: B =:= Fixed) =
      ElementStream(clip,
        stream.map {
          case Fixed((x, y), drawable) => Timed(0, Integer.MAX_VALUE, PositionDrawable(_ => (x, y), drawable))
        })

    def ++(other: ElementStream[None.type, B])(implicit ev: ElementStream[A, B] <:< ElementStream[None.type, B]): ElementStream[None.type, B] =
      ElementStream(ev(this).stream ++ other.stream)

    def clip(clip: Shape) = ElementStream(Some(clip), stream)
  }


  sealed class Element

  object Appended {
    def apply(image: BufferedImage): Appended = Appended(image.getWidth, ImageDrawable(image))
  }

  case class Appended(width: Double, drawable: Drawable) extends Element

  object Fixed {
    def apply(position: (Double, Double), image: BufferedImage): Fixed = Fixed(position, ImageDrawable(image))
  }

  case class Fixed(position: (Double, Double), drawable: Drawable) extends Element

  object Positioned {
    def apply(position: (Double, Double), image: BufferedImage): Positioned = Positioned(position, image.getWidth, ImageDrawable(image))
  }

  case class Positioned(position: (Double, Double), width: Double, drawable: Drawable) extends Element

  case class Timed(startFrame: Int, endFrame: Int, drawable: Drawable) extends Element

  trait Drawable {
    def draw(frame: Int, g: Graphics2D)
  }

  case class PositionDrawable(position: Int => (Double, Double), drawable: Drawable) extends Drawable {
    override def draw(frame: Int, g: Graphics2D): Unit = {
      val tmp = g.getTransform
      val (x, y) = position(frame)
      g.transform(AffineTransform.getTranslateInstance(x, y))
      drawable.draw(frame, g)
      g.setTransform(tmp)
    }
  }

  case class TransformDrawable(transform: Int => AffineTransform, drawable: Drawable) extends Drawable {
    override def draw(frame: Int, g: Graphics2D): Unit = {
      val tmp = g.getTransform
      g.transform(transform(frame))
      drawable.draw(frame, g)
      g.setTransform(tmp)
    }
  }

  case class ImageDrawable(img: BufferedImage, alignh: AlignH = AlignLeft, alignv: AlignV = AlignTop) extends Drawable {
    def draw(frame: Int, g: Graphics2D): Unit = {
      val tmp = g.getTransform
      g.transform(AffineTransform.getTranslateInstance((alignh match {
        case AlignLeft => 0.0
        case AlignRight => -1.0
        case AlignCenter => -0.5
      }) * img.getWidth, (alignv match {
        case AlignTop => 0.0
        case AlignBottom => -1.0
        case AlignMiddle => -0.5
      }) * img.getHeight))
      g.drawImage(img, 0, 0, null) // scalastyle:ignore null
      g.setTransform(tmp)
    }
  }

  case class SVGDrawable(uri: URI, height: Double, css: String, alignh: AlignH = AlignLeft, alignv: AlignV = AlignTop) extends Drawable {

    val docfactory = new SAXSVGDocumentFactory(XMLResourceDescriptor.getXMLParserClassName)
    val doc = docfactory.createDocument(uri.toString)
    val style = doc.createElementNS("http://www.w3.org/2000/svg", "style")

    style.setTextContent(css)
    doc.getDocumentElement.appendChild(style)

    val builder = new GVTBuilder()
    val ua = new UserAgentAdapter()
    val ctx = new BridgeContext(ua, new DocumentLoader(ua))
    val node = builder.build(ctx, doc)

    val bounds = node.getBounds
    val ratio = bounds.getWidth / bounds.getHeight
    val width = ratio * height
    val scale = height / bounds.getHeight
    val transform = AffineTransform.getTranslateInstance(-bounds.getX, -bounds.getY)
    transform.preConcatenate(AffineTransform.getScaleInstance(scale, scale))
    transform.preConcatenate(AffineTransform.getTranslateInstance((alignh match {
      case AlignLeft => 0.0
      case AlignRight => -1.0
      case AlignCenter => -0.5
    }) * width, (alignv match {
      case AlignTop => 0.0
      case AlignBottom => -1.0
      case AlignMiddle => -0.5
    }) * height))
    node.setTransform(transform)

    def draw(frame: Int, g: Graphics2D): Unit = {
      node.paint(g)
    }
  }

  def clipImage(image: BufferedImage, width: Int): Option[BufferedImage] = if (width > 0) {
    val result = new BufferedImage(width, image.getHeight, image.getType)
    result.getGraphics.drawImage(image, 0, 0, null) // scalastyle:ignore null
    Some(result)
  } else {
    None
  }

  def getAnim(uri: URI, height: Float, frames: Double, css: String): (IndexedSeq[Double], IndexedSeq[BufferedImage]) = {
    val framePositions: IndexedSeq[Double] = Source.fromURL(new URI(uri.toString + "/times.txt").toURL).getLines().map(_.toDouble).map(_ * frames).toIndexedSeq
    val images = for (i <- 0 to framePositions.size) yield getImage(new URI(uri.toString + "/" + i.toString + ".svg"), height, css)
    (framePositions, images)
  }

  case class AnimDrawable(offset: Double, anim: (IndexedSeq[Double], IndexedSeq[BufferedImage])) extends Drawable {
    val (framePositions, images) = anim
    val frameCount = framePositions.last.round.toInt

    override def draw(frame: Int, g: Graphics2D): Unit = {
      val i = frame + offset
      if (i < 0) {
        g.drawImage(images.head, 0, 0, null) // scalastyle:ignore null
      } else if (i < frameCount) {
        val j = framePositions.indexWhere(i < _)
        val distanceNext = (framePositions(j) - i)
        val positionLast = if (j == 0) 0.0 else framePositions(j - 1)
        val distanceLast = i - positionLast
        val duration = framePositions(j) - positionLast
        val ratioLast = 1.0 - distanceLast / duration
        val ratioNext = 1.0 - distanceNext / duration
        val img = new BufferedImage(images.head.getWidth, images.head.getHeight(), BufferedImage.TYPE_INT_ARGB)
        val g2 = img.createGraphics()
        g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, ratioLast.toFloat))
        g2.drawImage(images(j), 0, 0, null) // scalastyle:ignore null
        g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, ratioNext.toFloat))
        g2.drawImage(images(j + 1), 0, 0, null) // scalastyle:ignore null
        g.drawImage(img, 0, 0, null) // scalastyle:ignore null
      } else {
        g.drawImage(images.last, 0, 0, null) // scalastyle:ignore null
      }
    }
  }

  case class FramesDrawable(frames: IndexedSeq[Drawable]) extends Drawable {

    override def draw(frame: Int, g: Graphics2D): Unit = {
      if (frame < frames.size) {
        frames(frame).draw(frame, g)
      }
    }
  }

  def getImage(uri: URI, height: Float, css: String): BufferedImage = {
    val hints = new TranscodingHints()
    hints.put(SVGAbstractTranscoder.KEY_HEIGHT, height)
    hints.put(XMLAbstractTranscoder.KEY_XML_PARSER_VALIDATING, false)
    hints.put(XMLAbstractTranscoder.KEY_DOM_IMPLEMENTATION, SVGDOMImplementation.getDOMImplementation)
    hints.put(XMLAbstractTranscoder.KEY_DOCUMENT_ELEMENT_NAMESPACE_URI, SVGConstants.SVG_NAMESPACE_URI)
    hints.put(XMLAbstractTranscoder.KEY_DOCUMENT_ELEMENT, "svg")

    val docfactory = new SAXSVGDocumentFactory(XMLResourceDescriptor.getXMLParserClassName)
    val doc = docfactory.createDocument(uri.toString)
    val style = doc.createElementNS("http://www.w3.org/2000/svg", "style")

    style.setTextContent(css)
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
    rasterizer.transcode(input, null) // scalastyle:ignore null

    image.get
  }

  def getGraphicsNode(uri: URI, height: Float, css: String): GraphicsNode = {
    val docfactory = new SAXSVGDocumentFactory(XMLResourceDescriptor.getXMLParserClassName)
    val doc = docfactory.createDocument(uri.toString)
    val style = doc.createElementNS("http://www.w3.org/2000/svg", "style")

    style.setTextContent(css)
    doc.getDocumentElement.appendChild(style)

    val builder = new GVTBuilder
    val ctx = new BridgeContext(new UserAgentAdapter)
    builder.build(ctx, doc)
  }

  def getCSSColor(color: Color) = {
    val rgb = (color.getRGB & 0xffffff).toHexString
    "#" + "0" * (6 - rgb.length) + rgb
  }

  def getCSSColor(color: paint.Color) = {
    val rgb = ((color.red * 255).toInt << 16 | (color.green * 255).toInt << 8 | (color.blue * 255).toInt).toHexString
    "#" + "0" * (6 - rgb.length) + rgb
  }

  def getCSSOpacity(color: Color) = (color.getAlpha.toFloat / 0xff).toString()

  def getCSSOpacity(color: paint.Color) = color.opacity.toFloat.toString()

  def getImageLine(width: Int, pattern: (BufferedImage, BufferedImage, IndexedSeq[(BufferedImage, BufferedImage)])): ElementStream[None.type, Appended] = {
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
      clipImage(pattern._1, width).map(Appended(_)).toStream
    } else {
      val (endIdx, _, count) = counts.minBy(_._2)
      val imgs: Seq[Appended] = for {
        _ <- 0 until count
        img <- pattern._3
      } yield Appended(img._1)
      val endImgs = (pattern._3.slice(0, endIdx).map(x => x._1) :+ pattern._3(endIdx)._2).toStream
      val restWidth = width - patternWidth * count - endImgs.map(_.getWidth).sum - pattern._2.getWidth
      assert(restWidth >= 0, s"Expected >= 0 but was $restWidth")
      val extraImgStart = clipImage(pattern._1, restWidth / 2).map(Appended(_))
      val extraImgEnd = clipImage(pattern._1, restWidth - restWidth / 2).map(Appended(_))
      extraImgStart.toStream ++ Stream(Appended(pattern._2)) ++ imgs ++ endImgs.map(Appended(_)) ++ extraImgEnd.toStream
    }
    //assert(result.map(_.getWidth).sum - width == 0, s"Expected $width but was ${result.map(_.getWidth).sum}")
    ElementStream(None, result)
  }

  type TimedStream = ElementStream[Option[Shape], Beatmeter.Timed]
}

trait Beatmeter {

  def getElementStreams(beats: Seq[(Double, Boolean)], messages: Seq[(Double, Double, String)], frameCount: Int): List[Beatmeter.TimedStream]

  def minimalBeatDistance: Double

  def width: Int

  def height: Int

  def frames: Double
}
