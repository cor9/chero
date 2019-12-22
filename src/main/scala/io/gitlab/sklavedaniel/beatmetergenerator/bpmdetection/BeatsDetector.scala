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

package io.gitlab.sklavedaniel.beatmetergenerator.bpmdetection

import java.util
import java.util.LinkedList

import v4lk.lwbd.decoders.Decoder
import v4lk.lwbd.decoders.processing.fft.{FFT, FourierTransform}

object BeatsDetector {

  def calculateSpectralFluxes(decoder: Iterator[Array[Short]]) = new Iterator[Float] {
    val transformer = new FFT(1024, 44100)
    transformer.window(FourierTransform.HAMMING)
    val spectrumSize = (1024 / 2) + 1
    var currentSpectrum: Array[Float] = new Array[Float](spectrumSize)
    var previousSpectrum: Array[Float] = new Array[Float](spectrumSize)

    def hasNext = decoder.hasNext

    def next() = {
      val protoframe = decoder.next()
      val frame = protoframe.map(x => x.toFloat / 32768f)
      transformer.forward(frame)
      previousSpectrum = currentSpectrum
      currentSpectrum = Array(transformer.getSpectrum: _*)
      val flux = currentSpectrum.toIterator.zip(previousSpectrum.toIterator).map(x => x._1 + x._2).filter(_ > 0).sum
      flux
    }
  }

}
