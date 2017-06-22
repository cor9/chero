package io.gitlab.sklavedaniel.beatmetergenerator.bpmdetection

import java.io.{BufferedInputStream, File, FileInputStream}
import javax.sound.sampled.{AudioFormat, AudioSystem}

import io.gitlab.sklavedaniel.beatmetergenerator.{AudioPlayer, Main}

object BPMDetector {

  class Conf extends Main.ExecutableSubcommand("bpm") {
    val input = opt[File](required = true, descr = "Audio file to analyze")
    val from = opt[Double](descr = "Starting time")
    val to = opt[Double](descr = "End time")
    val haar = opt[Boolean](descr = "Haar instead of daubechies wavelets")
    val window = opt[Int](default = Some(17), descr = "Window size exponent, size will be set to 2^x, default is 17")
    val channel = opt[String](default = Some("both"), descr = "Channel to analyze: left, right, both")
    validateFileExists(input)

    def execute(args: Array[String]): Unit = {
      new BPMDetector(this)
    }
  }

}

class BPMDetector(conf: BPMDetector.Conf) {
  val detection = new WaveletBPMDetection(if(conf.haar()) WaveletBPMDetection.Haar else WaveletBPMDetection.Daubechies4, conf.window())

  val is = new BufferedInputStream(new FileInputStream(conf.input()))
  val format = new AudioFormat(44100, 16, 2, true, false)
  println("Loading data")
  val data = AudioPlayer.readData(is, format)
  val dataSlice = data.slice(conf.from.map(t => (t * format.getFrameRate * 2).toInt).getOrElse(0),
    conf.to.map(t => (t * format.getFrameRate * 2).toInt + 1).getOrElse(data.length))
  println("Generating input")
  val mapping = conf.channel() match {
    case "both" => (t: Seq[Double]) => t.sum / Short.MaxValue
    case "left" => (t: Seq[Double]) => t(0)
    case "right" => (t: Seq[Double]) => t(1)
  }
  val monoData = dataSlice.toIterator.map(_.toDouble).grouped(format.getChannels).map(mapping).toArray
  println("Analyzing")
  val (bpm, bpms) = detection.detect(monoData, format.getSampleRate, Some((t, d) => {
    println(f"$d%.3f in window starting at $t%.3f")
  }))
  println(f"Median bpm: $bpm%.3f")

}
