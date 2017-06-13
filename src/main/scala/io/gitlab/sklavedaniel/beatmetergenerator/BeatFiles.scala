package io.gitlab.sklavedaniel.beatmetergenerator

import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

object BeatFiles {
  def store(file: File, beats: Traversable[Double]): Unit = {
    val output = new BufferedWriter(new FileWriter(file))
    try {
      for (b <- beats) {
        val beat = b / 1000.0
        output.write(s"$beat $beat beat\n")
      }
    } finally
      output.close()
  }
  def load(file: File): Seq[Double] = {
    val result = Source.fromFile(file).getLines().map("\\s+".r.split(_).head.replace(",", ".").toDouble * 1000.0).toList
    assert(result == result.sorted, "Input file must be sorted.")
    result
  }
}

