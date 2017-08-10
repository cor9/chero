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

/*
 *  Except for minor changes the code in this file has been copied from
 *    https://github.com/mziccard/scala-audio-file/
 *  where it is available under a less restrictive license.
 *
 */

package io.gitlab.sklavedaniel.beatmetergenerator.bpmdetection

import jwave.Transform
import jwave.transforms.FastWaveletTransform
import jwave.transforms.wavelets.daubechies.Daubechies4
import jwave.transforms.wavelets.haar.Haar1

import scala.math.min
import scala.math.pow
import scala.math.abs
import ArrayOps._

/**
  * Class <code>WaveletBPMDetector</code> can be used to
  * detect the tempo of a track in beats-per-minute.
  * The class implements the algorithm presented by
  * Tzanetakis, Essl and Cookin the paper titled
  * "Audio Analysis using the Discrete Wavelet Transform"
  *
  * Objects of the class can be created using the companion
  * object's factory method.
  *
  * To detect the tempo the discrete wavelet transform is used.
  * Track samples are divided into windows of frames.
  * For each window data are divided into 4 frequency sub-bands
  * through DWT. For each frequency sub-band an envelope is
  * estracted from the detail coffecients by:
  * 1) Full wave rectification (take the absolute value),
  * 2) Downsampling of the coefficients,
  * 3) Normalization (via mean removal)
  * These 4 sub-band envelopes are then summed together.
  * The resulting collection of data is then autocorrelated.
  * Peaks in the correlated data correspond to peaks in the
  * original signal.
  * then peaks are identified on the filtered data.
  * Given the position of such a peak the approximated
  * tempo of the window is computed and appended to a colletion.
  * Once all windows in the track are processed the beat-per-minute
  * value is returned as the median of the windows values.
  *
  * Audio track data is buffered so that there's no need
  * to load the whole track in memory before applying
  * the detection.
  *
  * Class constructor is private, use the companion
  * object instead.
  **/
class WaveletBPMDetection(
  waveletType: WaveletBPMDetection.Wavelet,
  windowExponent: Int
) extends BPMDetection {

  val windowSize = Math.pow(2, windowExponent).toInt
  val wavelet = waveletType match {
    case WaveletBPMDetection.Haar => new Haar1()
    case WaveletBPMDetection.Daubechies4 => new Daubechies4()
  }


  /**
    * Identifies the location of data with the maximum absolute
    * value (either positive or negative). If multiple data
    * have the same absolute value the last positive is taken
    *
    * @param data the input array from which to identify the maximum
    * @return the index of the maximum value in the array
    **/
  private def detectPeak(data: Array[Double]): Int = {
    var max: Double = Double.MinValue

    for (x <- data) {
      if (abs(x) > max) max = abs(x)
    }
    var location = -1
    var i = 0
    while (i < data.length && location == -1) {
      if (data(i) == max) {
        location = i
      }
      i = i + 1
    }
    i = 0
    while (i < data.length && location == -1) {
      if (data(i) == -max) {
        location = i
      }
      i = i + 1
    }
    location
  }

  /**
    * Given <code>windowFrames</code> samples computes a BPM
    * value for the window and pushes it in <code>instantBpm</code>
    *
    * @param data An array of <code>windowFrames</code> samples representing the window
    **/
  private def computeWindowBpm(data: Array[Double], sampleRate: Double): Double = {

    var aC: Array[Double] = null // scalastyle:ignore null
    var dC: Array[Double] = null // scalastyle:ignore null
    var dCSum: Array[Double] = null // scalastyle:ignore null
    var dCMinLength: Int = 0
    val levels = 4
    val maxDecimation = pow(2, levels - 1)
    val minIndex: Int = (60.toDouble / 220 * sampleRate / maxDecimation).toInt
    val maxIndex: Int = (60.toDouble / 40 * sampleRate / maxDecimation).toInt

    // 4 Level DWT
    for (loop <- 0 until levels) {

      // Apply DWT
      val transform = new Transform(new FastWaveletTransform(wavelet))
      if (loop == 0) {
        val coefficients: Array[Array[Double]] = transform.decompose(data)
        val l = coefficients.length - 1
        aC = coefficients(1).slice(0, coefficients(1).length / 2)
        dC = coefficients(l).slice(coefficients(l).length / 2, coefficients(l).length)
        dCMinLength = (dC.length / maxDecimation).toInt + 1
      } else {
        val coefficients: Array[Array[Double]] = transform.decompose(aC)
        val l = coefficients.length - 1
        aC = coefficients(1).slice(0, coefficients(1).length / 2)
        dC = coefficients(l).slice(coefficients(l).length / 2, coefficients(l).length)
      }

      // Extract envelope from detail coefficients
      //  1) Undersample
      //  2) Absolute value
      //  3) Subtract mean
      val pace = pow(2, (levels - loop - 1)).toInt
      dC = dC.undersample(pace).abs
      dC = dC - dC.mean

      // Recombine detail coeffients
      if (dCSum == null) { // scalastyle:ignore null
        dCSum = dC.slice(0, dCMinLength)
      } else {
        dCSum = dC.slice(0, min(dCMinLength, dC.length)) |+| dCSum
      }
    }

    // Add the last approximated data
    aC = aC.abs
    aC = aC - aC.mean
    dCSum = aC.slice(0, min(dCMinLength, dC.length)) |+| dCSum

    // Autocorrelation
    var correlated: Array[Double] = dCSum.correlate
    val correlatedTmp = correlated.slice(minIndex, maxIndex)


    // Detect peak in correlated data
    val location = detectPeak(correlatedTmp)

    // Compute window BPM given the peak
    val realLocation = minIndex + location
    val windowBpm: Double = 60.toDouble / realLocation * (sampleRate / maxDecimation)
    windowBpm
  }


  override def detect(data: Array[Double], sampleRate: Double, onWindowAnalyzed: Option[(Double, Double) => Boolean]) = {
    val windowCount = (data.length.toDouble / windowSize).toInt
    var computing = true
    val bpms = for (i <- 0 until windowCount) yield {
      if (computing) {
        val time = i * windowSize.toDouble / sampleRate
        val bpm = computeWindowBpm(data.slice(i * windowSize, (i + 1) * windowSize), sampleRate)
        onWindowAnalyzed.foreach(f => computing = f(time, bpm))
        (time, bpm)
      } else {
        (0.0, 0.0)
      }
    }
    (bpms.map(_._2).sorted.apply(bpms.size / 2), bpms)
  }

}

object WaveletBPMDetection {

  sealed trait Wavelet

  case object Haar extends Wavelet

  case object Daubechies4 extends Wavelet

}

