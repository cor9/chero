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
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import javax.sound.sampled._

import org.apache.commons.io.IOUtils
import resource._

object AudioPlayer {

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
class AudioPlayer(audioData: InputStream, beatData: InputStream) {
  self =>

  import AudioPlayer.readData

  val bufferDuration = 0.1

  val format = new AudioFormat(44100, 16, 2, true, false)

  val beat = readData(beatData, format)
  val beatCount = beat.length / format.getChannels
  val audio = readData(audioData, format)
  val audioCount = audio.length / format.getChannels
  val duration = audioCount.toDouble / format.getFrameRate

  val avgDuration = 0.01
  val avgFrames = (avgDuration * format.getFrameRate * format.getChannels).round.toInt
  val avgTimes = (0 to audio.length / avgFrames).map(_ * avgDuration) :+ duration
  val avgs: Seq[(Double, Double)] = (Iterator.single(0.0) ++ audio.toIterable.grouped(avgFrames).map(l => l.map(_.abs.toDouble).sum / l.size)).zip(avgTimes.toIterator).toSeq
  val maxs: Seq[(Double, Double)] = (Iterator.single(0.0) ++ audio.toIterable.grouped(avgFrames).map(l => l.map(_.abs.toDouble).max)).zip(avgTimes.toIterator).toSeq

  var rate = new AtomicReference(0.5f)
  var position = new AtomicReference(0.0)
  var beats = new AtomicReference(Seq[Double]())
  var ratio = new AtomicReference(0.9)
  var listener: AtomicReference[Option[(Double, AudioPlayer.EventState) => Unit]] = new AtomicReference(None)

  private var audioThread = new AudioThread()
  audioThread.start()

  def currentPosition =
    if (audioThread.running.get()) {
      audioThread.startPosition.get() + audioThread.rate.get() * audioThread.sourceLine.getMicrosecondPosition / 1000000.0
    } else {
      position.get()
    }

  private class AudioThread extends Thread {
    var available = new AtomicBoolean(true)
    var running = new AtomicBoolean(false)
    val sourceLine = AudioSystem.getLine(new DataLine.Info(classOf[SourceDataLine], format)).asInstanceOf[SourceDataLine]
    var startPosition = new AtomicReference(self.position.get())
    var ratio = new AtomicReference(self.ratio.get())
    var rate = new AtomicReference(self.rate.get())

    override def run(): Unit = {
      while (available.get()) {
        synchronized(wait())
        startPosition.set(self.position.get())
        ratio.set(self.ratio.get())
        rate.set(self.rate.get())
        var position = (startPosition.get() * format.getFrameRate).round.toInt
        val bufferSize = (format.getFrameRate * bufferDuration * rate.get()).round.toInt
        var beats: Seq[(Int, Int)] = (self.beats.get().map(b => (b * format.getFrameRate).ceil.toInt) :+ audioCount)
          .sliding(2).filter(_.length == 2).map(x => (x(0), x(1))).dropWhile(_._2 <= position).toSeq
        val bbuffer = ByteBuffer.allocate(bufferSize * format.getFrameSize).order(if (format.isBigEndian) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)

        sourceLine.open(rateFormat(format, rate.get()))
        sourceLine.start()
        listener.get().foreach(_ (startPosition.get(), AudioPlayer.Playing))

        while (running.get() && position < audioCount) {
          bbuffer.clear()
          val l = bufferSize.min(audioCount - position)
          for {
            i <- 0 until l
            j <- 0 until format.getChannels
          } {
            bbuffer.putShort(((1.0 - ratio.get()) * audio((position + i) * format.getChannels + j)).toShort)
          }

          beats = beats.dropWhile(_._2 < position)
          for ((b1, b2) <- beats.takeWhile(_._1 <= position + bufferSize)) {
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
                  (bbuffer.getShort(k) + ratio.get() * beat((sourceStart + i) * format.getChannels + j)).toShort)
              }
            }
          }
          position += sourceLine.write(bbuffer.array(), 0, l * format.getFrameSize) / format.getFrameSize
          listener.get().foreach(_ (startPosition.get() + sourceLine.getMicrosecondPosition.toDouble / 1000000.0 * rate.get(), AudioPlayer.Played))
        }
        sourceLine.stop()
        listener.get().foreach(_ (startPosition.get() + sourceLine.getMicrosecondPosition.toDouble / 1000000.0 * rate.get(), AudioPlayer.Stopped))
        sourceLine.close()

        self.synchronized {
          self.notify()
        }
      }
    }
  }


  def playing = audioThread.running.get()

  def play(): Unit = {
    if (!audioThread.running.getAndSet(true)) {
      audioThread.synchronized(audioThread.notify())
    }
  }

  def pause(): Unit = {
    self.synchronized {
      if (audioThread.running.getAndSet(false)) {
        wait(100)
      }
    }
  }

  def terminate(): Unit = {
    audioThread.running.set(false)
    audioThread.available.set(false)
    audioThread.synchronized(audioThread.notify())
    audioThread.join()
  }

  private def rateFormat(format: AudioFormat, rate: Float) =
    new AudioFormat(format.getEncoding, rate * format.getSampleRate, format.getSampleSizeInBits, format.getChannels, format.getFrameSize, rate * format.getFrameRate, format.isBigEndian)

}
