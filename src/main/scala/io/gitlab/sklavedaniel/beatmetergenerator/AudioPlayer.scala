package io.gitlab.sklavedaniel.beatmetergenerator

import java.io.InputStream
import java.nio.{ByteBuffer, ByteOrder}
import javax.sound.sampled._

import org.apache.commons.io.IOUtils
import resource._

object AudioPlayer {

  sealed abstract class EventState

  case object Playing extends EventState

  case object Played extends EventState

  case object Stopped extends EventState

}

class AudioPlayer(audioData: InputStream, beatData: InputStream) {
  self =>

  val bufferDuration = 0.1

  val format = new AudioFormat(44100, 16, 2, true, false)

  val beat = readData(beatData, format)
  val beatCount = beat.length / format.getChannels
  val audio = readData(audioData, format)
  val audioCount = audio.length / format.getChannels
  val duration = audioCount.toDouble / format.getFrameRate

  val avgDuration = 0.05
  val avgFrames = (avgDuration * format.getFrameRate * format.getChannels).round.toInt
  val avgTimes = (0 to audio.length / avgFrames).map(_ * avgDuration) :+ duration
  val avgs: Seq[(Double, Double)] = (Iterator.single(0.0) ++ audio.toIterable.grouped(avgFrames).map(l => l.map(_.abs.toDouble).sum/ l.size)).zip(avgTimes.toIterator).toSeq

  var rate = 0.5f
  var position = 0.0
  var beats = Seq[Double]()
  var ratio = 0.9
  var listener: Option[(Double, AudioPlayer.EventState) => Unit] = None

  def currentPosition = {
    audioThread.map {
      thread =>
        thread.startPosition + thread.rate * thread.sourceLine.getMicrosecondPosition / 1000000.0
    }.getOrElse(position)
  }

  private class AudioThread extends Thread {
    @volatile
    var running = true
    val sourceLine = AudioSystem.getLine(new DataLine.Info(classOf[SourceDataLine], format)).asInstanceOf[SourceDataLine]
    val startPosition = self.position
    val ratio = self.ratio
    val rate = self.rate

    override def run(): Unit = {
      var position = (startPosition * format.getFrameRate).round.toInt
      val bufferSize = (format.getFrameRate * bufferDuration * rate).round.toInt
      var beats: Seq[(Int, Int)] = (self.beats.map(b => (b * format.getFrameRate).ceil.toInt) :+ audioCount)
        .sliding(2).filter(_.length == 2).map(x => (x(0), x(1))).dropWhile(_._2 <= position).toSeq
      val bbuffer = ByteBuffer.allocate(bufferSize * format.getFrameSize).order(if (format.isBigEndian) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)

      sourceLine.open(rateFormat(format, rate))
      listener.foreach(_ (startPosition, AudioPlayer.Playing))
      sourceLine.start()

      while (running && position < audioCount) {
        bbuffer.clear()
        val l = bufferSize.min(audioCount - position)
        for {
          i <- 0 until l
          j <- 0 until format.getChannels
        } {
          bbuffer.putShort(((1.0 - ratio) * audio((position + i) * format.getChannels + j)).toShort)
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
                (bbuffer.getShort(k) + ratio * beat((sourceStart + i) * format.getChannels + j)).toShort)
            }
          }
        }

        position += sourceLine.write(bbuffer.array(), 0, l * format.getFrameSize) / format.getFrameSize
        listener.foreach(_ (startPosition + sourceLine.getMicrosecondPosition.toDouble / 1000000.0 * rate, AudioPlayer.Played))
      }
    }
  }

  private var audioThread: Option[AudioThread] = None


  def playing = audioThread.isDefined

  def play(): Unit = {
    if (audioThread.isEmpty) {
      val thread = new AudioThread()
      thread.start()
      audioThread = Some(thread)
    }
  }

  def pause(): Unit = {
    audioThread.foreach { thread =>
      thread.running = false
      thread.sourceLine.stop()
      listener.foreach(_ (thread.startPosition + thread.sourceLine.getMicrosecondPosition.toDouble / 1000000.0 * thread.rate, AudioPlayer.Stopped))
      thread.join()
      thread.sourceLine.close()
    }
    audioThread = None
  }

  private def readData(data: InputStream, format: AudioFormat): Array[Short] = {
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

  private def rateFormat(format: AudioFormat, rate: Float) =
    new AudioFormat(format.getEncoding, rate * format.getSampleRate, format.getSampleSizeInBits, format.getChannels, format.getFrameSize, rate * format.getFrameRate, format.isBigEndian)

}
