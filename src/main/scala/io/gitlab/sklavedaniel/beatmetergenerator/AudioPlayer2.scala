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

import java.io.InputStream
import java.nio.{ByteBuffer, ByteOrder}
import java.util.concurrent.{FutureTask, LinkedBlockingQueue}
import javax.sound.sampled._

import org.apache.commons.io.IOUtils
import resource._

import scalafx.application.Platform
import scalafx.beans.property._

object AudioPlayer2 {

  sealed abstract class EventState

  case object Playing extends EventState

  case object Played extends EventState

  case object Stopped extends EventState

  def readData(data: InputStream, format: AudioFormat): Array[Short] = {
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
    }).tried.get
  }
}

class AudioPlayer2(audioData: InputStream, beatData: InputStream) {
  self =>

  import AudioPlayer.readData

  val bufferDuration = 0.1

  val format = new AudioFormat(44100, 16, 2, true, false)

  val beat = readData(beatData, format)
  val beatCount = beat.length / format.getChannels
  println("loading audio file...")
  val audio = readData(audioData, format)
  val audioCount = audio.length / format.getChannels
  val duration = audioCount.toDouble / format.getFrameRate
  println("audio file loaded.")

  val maximaDuration = 0.025
  val maximaFrames = (maximaDuration * format.getFrameRate * format.getChannels).round.toInt
  val maximaTimes = (0 to audio.length / maximaFrames).map(_ * maximaDuration) :+ duration
  val maxima: Seq[((Double, Double), Double)] = (Iterator.single((0.0, 0.0)) ++ audio.toIterable.grouped(maximaFrames).map {
    l => (l.grouped(2).map(_.head.abs.toDouble).max, l.grouped(2).map(_.last.abs.toDouble).max)
  }).zip(maximaTimes.toIterator).toSeq

  private val sync = new LinkedBlockingQueue[Unit]()
  private val syncChange = (_: Any, _: Any, _: Any) => if (sync.isEmpty) {
    sync.offer(())
  }: Unit

  val rate = FloatProperty(0.5f)
  rate.onChange(syncChange)
  val position = ObjectProperty((0.0, true))
  position.onChange { (_, _, x) =>
    if (x._2 && sync.isEmpty) {
      sync.offer(())
    }
  }
  val beats = ObjectProperty(List[Double]())
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

        val task = new FutureTask[(Double, Double, Float, Boolean, List[Double])](() => (self.position()._1, self.ratio(), self.rate(), self.playing(), self.beats()))
        Platform.runLater(task)
        val (currentPosition, currentRatio, currentRate, currentPlaying, currentBeats) = task.get

        if (currentPlaying) {
          var position = (currentPosition * format.getFrameRate).round.toInt
          val bufferSize = (format.getFrameRate * bufferDuration * currentRate).round.toInt
          var bs: Seq[(Int, Int)] = (currentBeats.map(b => (b * format.getFrameRate).ceil.toInt) :+ audioCount)
            .sliding(2).filter(_.length == 2).map(x => (x(0), x(1))).dropWhile(_._2 <= position).toSeq
          val bbuffer = ByteBuffer.allocate(bufferSize * format.getFrameSize).order(if (format.isBigEndian) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)

          sourceLine.open(rateFormat(format, currentRate))
          sourceLine.start()
          updatePosition(currentPosition + sourceLine.getMicrosecondPosition.toDouble / 1000000.0 * currentRate, true)

          while (sync.isEmpty && position < audioCount) {
            bbuffer.clear()
            val l = bufferSize.min(audioCount - position)
            for {
              i <- 0 until l
              j <- 0 until format.getChannels
            } {
              bbuffer.putShort(((1.0 - currentRatio) * audio((position + i) * format.getChannels + j)).toShort)
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
          updatePosition(currentPosition + sourceLine.getMicrosecondPosition.toDouble / 1000000.0 * currentRate, false)
          sourceLine.stop()
          sourceLine.close()
        }
      }
    }
  }

  private def rateFormat(format: AudioFormat, rate: Float) =
    new AudioFormat(format.getEncoding, rate * format.getSampleRate, format.getSampleSizeInBits, format.getChannels, format.getFrameSize, rate * format.getFrameRate, format.isBigEndian)

}
