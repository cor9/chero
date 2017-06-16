package io.gitlab.sklavedaniel.beatmetergenerator

import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

object BeatFiles {
  def store(file: File, beats: Traversable[Double]): Unit = {
    val output = new BufferedWriter(new FileWriter(file))
    try {
      for (b <- beats) {
        output.write(s"$b $b beat\n")
      }
    } finally
      output.close()
  }

  def load(file: File): Seq[Double] = {
    val result = Source.fromFile(file).getLines().flatMap { l =>
      val parts = "\\s+".r.split(l)
      val time = parts(0).replace(",", ".").toDouble
      if (parts(2) == "beat")
        Some(time)
      else
        None
    }.toList
    assert(result == result.sorted, "Input file must be sorted.")
    result
  }
}

