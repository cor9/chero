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

package io.gitlab.sklavedaniel.beatmetergenerator.editor

import java.io.{BufferedInputStream, InputStream}
import java.nio.{ByteBuffer, ByteOrder}
import java.util.concurrent.{FutureTask, LinkedBlockingQueue}

import io.gitlab.sklavedaniel.beatmetergenerator.editor.AudioPlayer.BeatInfo
import io.gitlab.sklavedaniel.beatmetergenerator.utils.{ObservableIntervalMap, _}
import javafx.beans.InvalidationListener
import javax.sound.sampled._
import org.apache.commons.io.IOUtils
import resource._
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.beans.binding.Bindings
import scalafx.beans.property._
import scalafx.collections.ObservableBuffer
import scalafx.collections.ObservableBuffer.{Add, Remove}
import scalafx.stage.Window

object AudioPlayer {

  sealed abstract class EventState

  case object Playing extends EventState

  case object Played extends EventState

  case object Stopped extends EventState

  val format = new AudioFormat(44100, 16, 2, true, false)

  def readData(data: InputStream): WithFailures[Array[Short], Throwable] = {
    WithFailures.fromTry((for {
      ais2 <- managed(AudioSystem.getAudioInputStream(format, AudioSystem.getAudioInputStream(data)))
    } yield {
      val array = IOUtils.toByteArray(ais2)
      val buffer = ByteBuffer.allocate(array.length).order(if (format.isBigEndian) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
      buffer.put(array)
      buffer.rewind()
      val result = new Array[Short](buffer.limit() / 2)
      buffer.asShortBuffer().get(result)
      result
    }).tried)
  }

  class BeatInfo {
    val beat = ObjectProperty(Array[Short]())
    val beatCount = Bindings.createObjectBinding(() => beat().length / format.getChannels, beat)
    val beatDuration = Bindings.createDoubleBinding(() => beatCount() / format.getFrameRate, beatCount)
  }

  val defaultBeat = readData(new BufferedInputStream(getClass.getResourceAsStream("/beats/click.wav"))).get
}

class AudioPlayer() {
  self =>

  import AudioPlayer.format

  val progressDialogWindow = ObjectProperty[Option[Window]](None)

  val bufferDuration = 0.1

  val audio = ObjectProperty[Option[Array[Short]]](None)
  val audioCount = Bindings.createObjectBinding[Int](() => audio().map(_.length / format.getChannels()).getOrElse(0), audio)
  val audioDuration = Bindings.createDoubleBinding(() => audioCount().toDouble / format.getFrameRate, audioCount)

  val maximaDuration = 0.02
  val maximaFrames = (maximaDuration * format.getFrameRate * format.getChannels).round.toInt

  val maxima = Bindings.createObjectBinding[Option[(Array[Float], Array[Float])]](() => audio().map { a =>
    def compute(callback: (Option[Double], Option[String]) => Boolean) = {
      val leftResult = new Array[Float](a.length / maximaFrames)
      val rightResult = new Array[Float](a.length / maximaFrames)
      val limit = a.length / maximaFrames
      for (i <- 0 until limit) {
        if (i % 1000 == 0) {
          callback(Some(i.toFloat / limit), None)
        }
        leftResult(i) = (for (j <- 0 until maximaFrames / 2) yield a(maximaFrames * i + 2 * j).toFloat.abs).sum / maximaFrames * 2
        rightResult(i) = (for (j <- 0 until maximaFrames / 2) yield a(maximaFrames * i + 2 * j + 1).toFloat.abs).sum / maximaFrames * 2
      }
      (leftResult, rightResult)
    }

    if (progressDialogWindow().isDefined) {
      val task = (callback: (Option[Double], Option[String]) => Boolean) => {
        WithFailures.success(Some(compute(callback)))
      }
      val pd = new ProgressDialog[(Array[Float], Array[Float])](progressDialogWindow(), "Analyzing audio", Some("analyzing..."), false, task)
      pd.showAndWait().get.asInstanceOf[WithFailures[Option[(Array[Float], Array[Float])], Throwable]] match {
        case WithFailures(Some(Some(result)), _) =>
          result
        case _ =>
          assert(false); ???
      }
    } else {
      compute((_, _) => true)
    }
  }, audio)


  private val sync = new LinkedBlockingQueue[Unit]()
  private val syncChange = (_: Any, _: Any, _: Any) => if (sync.isEmpty) {
    sync.offer(())
  }: Unit

  val rate = FloatProperty(1.0f)
  rate.onChange(syncChange)
  val position = ObjectProperty((0.0, true))
  position.onChange {
    (_, _, x) =>
      if (x._2 && sync.isEmpty) {
        sync.offer(())
      }
  }

  val beats = ObservableBuffer[(BooleanProperty, BeatInfo, ObservableIntervalMap[Double, Beat])]()
  private val beatsDuration_ = ReadOnlyDoubleWrapper(0.0)
  val beatsDuration = beatsDuration_.readOnlyProperty

  private def calcBeatsInfo(): Unit = {
    beatsDuration_() = (Iterator(0.0) ++ (for {
      (b, info, map) <- beats
      if b()
      (d, _, _) <- map.lastOption
    } yield d + info.beatDuration.doubleValue())).max
    beatsCount_() = (beatsDuration.doubleValue() * format.getFrameRate).ceil.toInt
  }

  private val beatsCount_ = ReadOnlyIntegerWrapper(0)
  val beatsListener: InvalidationListener = _ => {
    calcBeatsInfo()
  }
  val beatsCount = beatsCount_.readOnlyProperty
  beats.onChange { (_, cs) =>
    calcBeatsInfo()
    for (c <- cs) {
      c match {
        case Add(i, as) =>
          for (a <- as) {
            a._3.addListener(beatsListener)
            a._2.beatDuration.addListener(beatsListener)
            a._1.addListener(beatsListener)
          }
        case Remove(i, rs) =>
          for (r <- rs) {
            r._3.removeListener(beatsListener)
            r._1.removeListener(beatsListener)
          }
        case _ =>
      }
    }
  }

  val duration = Bindings.createDoubleBinding(() => audioDuration.doubleValue().max(beatsDuration.doubleValue()), audioDuration, beatsDuration)
  val count = Bindings.createObjectBinding[Int](() => audioCount().max(beatsCount()), audioCount, beatsCount)


  val ratio = DoubleProperty(0.5)
  ratio.onChange(syncChange)
  val playing = BooleanProperty(false)
  playing.onChange(syncChange)

  private def computeCurrentBeats(currentPosition: Double) =
    beats.toArray.flatMap {
      case (enabled, info, map) if enabled() =>
        val tmp = (map.toList.map(b => (b._1 * format.getFrameRate).ceil.toInt) ++ map.lastOption.map(x => ((x._1 * format.getFrameRate).ceil.toInt + info.beatCount())))
          .sliding(2).filter(_.length == 2).map(x => (x(0), x(1))).dropWhile(_._2 <= (currentPosition * format.getFrameRate).round.toInt).toSeq
        Some((info.beatCount(), info.beat(), tmp))
      case _ =>
        None
    }

  private var audioThread = new AudioThread()
  audioThread.start()

  private class AudioThread extends Thread {
    setDaemon(true)

    def updatePosition(pos: Double, overwrite: Boolean): Unit = {
      if (overwrite) {
        Platform.runLater(self.position() = (pos, false))
      } else {
        Platform.runLater {
          if (!self.position()._2) {
            self.position() = (pos, false)
          }
        }
      }
    }

    override def run(): Unit = {
      val sourceLine = AudioSystem.getLine(new DataLine.Info(classOf[SourceDataLine], format)).asInstanceOf[SourceDataLine]
      while (true) {
        sync.take()

        val task = new FutureTask[(Double, Double, Float, Boolean, Array[(Int, Array[Short], Seq[(Int, Int)])], Option[Array[Short]], Int, Int)](() => {
          (self.position()._1, self.ratio(), self.rate(), self.playing(), computeCurrentBeats(self.position()._1), audio(), audioCount(), count())
        })
        Platform.runLater(task)
        val (currentPosition, currentRatio, currentRate, currentPlaying, currentBeats, currentAudio, currentAudioCount, currentCount) = task.get


        if (currentPlaying) {
          var pos = (currentPosition * format.getFrameRate).round.toInt
          val bufferSize = (format.getFrameRate * bufferDuration * currentRate).round.toInt
          var bs = currentBeats.map(_._3)
          val bbuffer = ByteBuffer.allocate(bufferSize * format.getFrameSize).order(if (format.isBigEndian) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)

          sourceLine.open(rateFormat(format, currentRate))
          sourceLine.start()
          val lineOffset = sourceLine.getMicrosecondPosition
          updatePosition(currentPosition + (sourceLine.getMicrosecondPosition - lineOffset).toDouble / 1000000.0 * currentRate, true)

          while (sync.isEmpty && pos < currentCount) {
            val (newbs, l) = fillBuffer(bufferSize, pos, currentCount, currentAudio, currentAudioCount, currentRatio, bbuffer, bs, currentBeats)
            bs = newbs
            pos += sourceLine.write(bbuffer.array(), 0, l * format.getFrameSize) / format.getFrameSize
            updatePosition(currentPosition + (sourceLine.getMicrosecondPosition - lineOffset).toDouble / 1000000.0 * currentRate, false)
          }
          sourceLine.drain()
          updatePosition(currentPosition + (sourceLine.getMicrosecondPosition - lineOffset).toDouble / 1000000.0 * currentRate, false)
          sourceLine.stop()
          sourceLine.close()
          if (pos >= currentCount) {
            Platform.runLater(playing() = false)
          }
        }
      }
    }
  }

  private def fillBuffer(bufferSize: Int, pos: Int, currentCount: Int, currentAudio: Option[Array[Short]], currentAudioCount: Int,
    currentRatio: Double, bbuffer: ByteBuffer, bs: Array[Seq[(Int, Int)]], currentBeats: Array[(Int, Array[Short], Seq[(Int, Int)])]
  ) = {
    bbuffer.clear()
    val l = bufferSize.min(currentCount - pos)
    for {
      i <- 0 until l
      j <- 0 until format.getChannels
    } {
      if (currentAudio.isDefined && pos + i < currentAudioCount) {
        bbuffer.putShort(((1.0 - currentRatio) * currentAudio.get((pos + i) * format.getChannels + j)).toShort)
      } else {
        bbuffer.putShort(0)
      }
    }

    val newbs = bs.map(_.dropWhile(_._2 < pos))
    for (bi <- bs.indices) {
      for ((b1, b2) <- bs(bi).takeWhile(_._1 <= pos + l)) {
        val targetStart = (b1 - pos).max(0)
        val sourceStart = (pos - b1).max(0)
        val len = (bufferSize - targetStart).min(currentBeats(bi)._1 - sourceStart)
        if (len > 0) {
          for {
            i <- 0 until len
            j <- 0 until format.getChannels
          } {
            val k = ((targetStart + i) * format.getChannels + j) * format.getFrameSize / format.getChannels
            bbuffer.putShort(k,
              (bbuffer.getShort(k) + currentRatio * currentBeats(bi)._2((sourceStart + i) * format.getChannels + j)).toShort)
          }
        }
      }
    }
    (newbs, l)
  }

  def generateStream(): ((Double => Boolean) => InputStream, Long) = {
    val (currentBeats, currentCount) = (computeCurrentBeats(0), count())
    ((callback: Double => Boolean) => new InputStream {
      var pos = 0
      val bufferSize = (format.getFrameRate * bufferDuration).round.toInt
      var bs = currentBeats.map(_._3)
      val bbuffer = ByteBuffer.allocate(bufferSize * format.getFrameSize).order(if (format.isBigEndian) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
      var bbufferPos = 0
      var bbufferLimit = 0

      private def generate(): Unit = {
        val (newbs, l) = fillBuffer(bufferSize, pos, currentCount, None, 0, 1.0, bbuffer, bs, currentBeats)
        bs = newbs
        pos += l
        bbufferPos = 0
        bbufferLimit = l * format.getFrameSize
      }

      generate()

      override def read(): Int = {
        if (bbufferPos != bbufferLimit) {
          val result = bbuffer.get(bbufferPos) & 0xff
          bbufferPos += 1
          if (bbufferPos == bbufferLimit) {
            if(callback(pos.toDouble / currentCount)) {
              generate()
            }
          }
          result
        } else {
          -1
        }
      }
    }, currentCount)
  }

  private def rateFormat(format: AudioFormat, rate: Float) =
    new AudioFormat(format.getEncoding, rate * format.getSampleRate, format.getSampleSizeInBits, format.getChannels, format.getFrameSize, rate * format.getFrameRate, format.isBigEndian)

}
