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

import java.util.concurrent.FutureTask

import io.gitlab.sklavedaniel.beatmetergenerator.utils.WithFailures
import javafx.scene
import javafx.scene.control
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.scene.control.{Button, Dialog, DialogPane, ProgressBar}
import scalafx.scene.layout.{HBox, Priority, VBox}
import scalafx.scene.text.Text
import scalafx.stage.Window

class ProgressDialog[A](ownerWindow: Option[Window], taskTitle: String, message: Option[String], cancelable: Boolean, task: ((Option[Double], Option[String]) => Boolean) => WithFailures[Option[A], Throwable]) extends Dialog[WithFailures[Option[A], Throwable]] {
  self =>
  title = "Beatmeter Generator"
  resizable = true
  width = 200
  height = 150
  ownerWindow.foreach(initOwner)
  val messageNode = new Text
  private var computing = true
  message.foreach(messageNode.text = _)
  val progress = new ProgressBar {
    hgrow = Priority.Always
    maxWidth = Double.PositiveInfinity
  }
  dialogPane = new DialogPane(new control.DialogPane {
    override def createButtonBar() = new scene.Group()
  }) {
    minWidth = 300
    headerText = taskTitle
    content = new VBox {
      spacing = 5
      children = Seq(
        messageNode,
        progress,
        new HBox(new HBox {
          hgrow = Priority.Always
        }, new Button("Cancel") {
          disable = !cancelable
          onAction = handle {
            disable = true
            computing = false
          }
        })
      )
    }
  }

  onShown = _ => {
    val thread = new Thread(() => {
      val r = task((d, s) => {
        val task = new FutureTask[Boolean](() => {
          d.foreach(progress.progress() = _)
          s.foreach(messageNode.text = _)
          computing
        })
        Platform.runLater(task)
        task.get()
      })
      Platform.runLater(() => {
        progress.progress() = 1.0
        result() = r.map(opt => if (computing) {
          opt
        } else {
          None
        })
        self.hide()
      })
    })
    thread.start()
  }
}
