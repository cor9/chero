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

import javafx.beans.value.{ChangeListener, WeakChangeListener}
import javafx.beans.{InvalidationListener, WeakInvalidationListener}
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.text.Text

import scala.collection.mutable.ListBuffer
import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.scene.input.MouseEvent
import scalafx.stage.Window
import scalafx.util.Duration

object Utils {

  def mouseHandler(single: MouseEvent => Unit, double: MouseEvent => Unit) = {
    var clickDelay: Option[Timeline] = None
    (e: MouseEvent) => {
      clickDelay.foreach(_.stop())
      clickDelay = None
      if (e.getClickCount == 1) {
        val t = Timeline(KeyFrame(Duration(300), onFinished = _ => {
          single(e)
        }))
        t.play()
        clickDelay = Some(t)
      } else if (e.getClickCount == 2) {
        double(e)
      }
    }
  }

  def weak[A](listener: ChangeListener[A]): ChangeListener[A] = new WeakChangeListener[A](listener)

  def weak(listener: InvalidationListener): InvalidationListener = new WeakInvalidationListener(listener)

  def formatTime(time: Double) = {
    val ms = (time * 1000).round
    val minutes = ms / (1000 * 60)
    val seconds = ms % (1000 * 60) / 1000
    val milis = ms % 1000
    f"$minutes%02d:$seconds%02d.$milis%03d"
  }

  def merge[A, B: Ordering](seq: Seq[Traversable[A]], f: A => B): List[A] = {
    def impl(seq: Seq[Traversable[A]], result: ListBuffer[A]): List[A] = {
      if (seq.isEmpty) {
        result.toList
      } else {
        val (trv, idx: Int) = seq.zipWithIndex.minBy(s => s._1.headOption.map(f))
        if (trv.isEmpty) {
          impl(seq.slice(0, idx) ++ seq.slice(idx + 1, seq.size), result)
        } else {
          result += trv.head
          impl(seq.slice(0, idx) ++ (trv.tail +: seq.slice(idx + 1, seq.size)), result)
        }
      }
    }

    impl(seq, ListBuffer.empty)
  }

  def alert(header: String, message: String, window: Window) = {
    val alert = new Alert(AlertType.Error) {
      title = "Beatmeter Generator"
      headerText = header
      dialogPane().content = new Text(message) {
        wrappingWidth = 500
      }
    }
    alert.initOwner(window)
    alert.showAndWait()
  }
}
