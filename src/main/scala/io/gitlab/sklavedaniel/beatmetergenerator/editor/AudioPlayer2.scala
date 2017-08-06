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

import java.io.InputStream
import java.nio.{ByteBuffer, ByteOrder}
import java.util.concurrent.{FutureTask, LinkedBlockingQueue}
import javafx.beans.InvalidationListener
import javax.sound.sampled._

import io.gitlab.sklavedaniel.beatmetergenerator.utils.ObservableIntervalMap
import org.apache.commons.io.IOUtils
import resource._

import scala.collection.mutable.ListBuffer
import scala.util.Try
import scalafx.application.Platform
import scalafx.beans.binding.Bindings
import scalafx.beans.property._
import scalafx.collections.ObservableBuffer
import scalafx.collections.ObservableBuffer.{Add, Remove}

object AudioPlayer2 {

  sealed abstract class EventState

  case object Playing extends EventState

  case object Played extends EventState

  case object Stopped extends EventState

  val format = new AudioFormat(44100, 16, 2, true, false)

  def readData(data: InputStream): Try[Array[Short]] = {
    (for {
      ais2 <- managed(AudioSystem.getAudioInputStream(format, AudioSystem.getAudioInputStream(data)))
    } yield {
      val array = IOUtils.toByteArray(ais2)
      val buffer = ByteBuffer.allocate(array.length).order(if (format.isBigEndian) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
      buffer.put(array)
      buffer.rewind()
      val result = new Array[Short](buffer.limit() / 2)
      buffer.asShortBuffer().get(result)
      result
    }).tried
  }
}

class AudioPlayer2(beatData: InputStream) {
  self =>

  import AudioPlayer2.format

  val bufferDuration = 0.1


  val beat = AudioPlayer2.readData(beatData).get
  val beatCount = beat.length / format.getChannels
  val beatDuration = beatCount.toDouble / format.getFrameRate

  val audio = ObjectProperty[Option[Array[Short]]](None)
  val audioCount = Bindings.createObjectBinding[Int](() => audio().map(_.length / format.getChannels()).getOrElse(0), audio)
  val audioDuration = Bindings.createObjectBinding[Double](() => audioCount().toDouble / format.getFrameRate, audioCount)

  val maximaDuration = 0.025
  val maximaFrames = (maximaDuration * format.getFrameRate * format.getChannels).round.toInt

  val maxima = Bindings.createObjectBinding[Option[Array[((Double, Double), Double)]]](() => audio().map { a =>
    val result = new Array[((Double, Double), Double)](a.length / maximaFrames)
    for (i <- 0 until a.length / maximaFrames) {
      val maxLeft = (for (j <- 0 until maximaFrames / 2) yield a(maximaFrames * i + 2 * j)).max
      val maxRight = (for (j <- 0 until maximaFrames / 2) yield a(maximaFrames * i + 2 * j + 1)).max
      val time = i * maximaDuration
      result(i) = ((maxLeft, maxRight), time)
    }
    result
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
  val beats = ObservableBuffer[(BooleanProperty, ObservableIntervalMap[Double, Beat])]()
  private val beatsDuration_ = ReadOnlyDoubleWrapper(0.0)
  val beatsDuration = beatsDuration_.readOnlyProperty

  private def calcBeatsInfo(): Unit = {
    beatsDuration_() = (Iterator(0.0) ++ (for {
      (_, map) <- beats
      (d, _, _) <- map.lastOption
    } yield d + beatDuration)).max
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
            a._2.addListener(beatsListener)
          }
        case Remove(i, rs) =>
          for (r <- rs) {
            r._2.removeListener(beatsListener)
          }
        case _ =>
      }
    }
  }

  val duration = Bindings.createObjectBinding[Double](() => audioDuration().max(beatsDuration()), audioDuration, beatsDuration)
  val count = Bindings.createObjectBinding[Int](() => audioCount().max(beatsCount()), audioCount, beatsCount)


  val ratio = DoubleProperty(0.9)
  ratio.onChange(syncChange)
  val playing = BooleanProperty(false)
  playing.onChange(syncChange)

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

        val task = new FutureTask[(Double, Double, Float, Boolean, Seq[(Int, Int)], Option[Array[Short]], Int, Int)](() => {
          var position = (self.position()._1 * format.getFrameRate).round.toInt
          val l = merge(self.beats.filter(_._1()).map(_._2.map(_._1)).toList, ListBuffer.empty)
          val tmp = (l.map(b => (b * format.getFrameRate).ceil.toInt) ++ l.lastOption.map(x => ((x * format.getFrameRate).ceil.toInt + beatCount)))
            .sliding(2).filter(_.length == 2).map(x => (x(0), x(1))).dropWhile(_._2 <= position).toSeq
          (self.position()._1, self.ratio(), self.rate(), self.playing(), tmp, audio(), audioCount(), count())
        })
        Platform.runLater(task)
        val (currentPosition, currentRatio, currentRate, currentPlaying, currentBeats, currentAudio, currentAudioCount, currentCount) = task.get

        if (currentPlaying) {
          var position = (currentPosition * format.getFrameRate).round.toInt
          val bufferSize = (format.getFrameRate * bufferDuration * currentRate).round.toInt
          var bs = currentBeats
          val bbuffer = ByteBuffer.allocate(bufferSize * format.getFrameSize).order(if (format.isBigEndian) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)

          sourceLine.open(rateFormat(format, currentRate))
          sourceLine.start()
          updatePosition(currentPosition + sourceLine.getMicrosecondPosition.toDouble / 1000000.0 * currentRate, true)

          while (sync.isEmpty && position < currentCount) {
            bbuffer.clear()
            val l = bufferSize.min(currentCount - position)
            for {
              i <- 0 until l
              j <- 0 until format.getChannels
            } {
              if (currentAudio.isDefined && position < currentAudioCount) {
                bbuffer.putShort(((1.0 - currentRatio) * currentAudio.get((position + i) * format.getChannels + j)).toShort)
              } else {
                bbuffer.putShort(0)
              }
            }

            bs = bs.dropWhile(_._2 < position)
            for ((b1, b2) <- bs.takeWhile(_._1 <= position + bufferSize)) {
              val targetStart = (b1 - position).max(0)
              val sourceStart = (position - b1).max(0)
              val len = (bufferSize - targetStart).min(beatCount - sourceStart)
              if (len > 0) {
                for {
                  i <- 0 until len
                  j <- 0 until format.getChannels
                } {
                  val k = ((targetStart + i) * format.getChannels + j) * format.getFrameSize / format.getChannels
                  bbuffer.putShort(k,
                    (bbuffer.getShort(k) + currentRatio * beat((sourceStart + i) * format.getChannels + j)).toShort)
                }
              }
            }
            position += sourceLine.write(bbuffer.array(), 0, l * format.getFrameSize) / format.getFrameSize
            updatePosition(currentPosition + sourceLine.getMicrosecondPosition.toDouble / 1000000.0 * currentRate, false)
          }
          sourceLine.drain()
          updatePosition(currentPosition + sourceLine.getMicrosecondPosition.toDouble / 1000000.0 * currentRate, false)
          sourceLine.stop()
          sourceLine.close()
          if (position >= currentCount) {
            Platform.runLater(playing() = false)
          }
        }
      }
    }
  }

  private def rateFormat(format: AudioFormat, rate: Float) =
    new AudioFormat(format.getEncoding, rate * format.getSampleRate, format.getSampleSizeInBits, format.getChannels, format.getFrameSize, rate * format.getFrameRate, format.isBigEndian)

  private def merge[A: Ordering](seq: Seq[Traversable[A]], result: ListBuffer[A]): List[A] = {
    if (seq.isEmpty) {
      result.toList
    } else {
      val (trv, idx: Int) = seq.zipWithIndex.minBy(_._1.headOption)
      if (trv.isEmpty) {
        merge(seq.slice(0, idx) ++ seq.slice(idx + 1, seq.size), result)
      } else {
        result += trv.head
        merge(seq.slice(0, idx) ++ (trv.tail +: seq.slice(idx + 1, seq.size)), result)
      }
    }
  }
}
