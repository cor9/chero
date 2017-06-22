package io.gitlab.sklavedaniel.beatmetergenerator.bpmdetection

trait BPMDetection {

  def detect(data: Array[Double], sampleRate: Double,
    onWindowAnalyzed: Option[(Double, Double) => Unit]): (Double, Seq[(Double, Double)])
}
