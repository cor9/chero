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

import io.gitlab.sklavedaniel.beatmetergenerator.Information
import io.gitlab.sklavedaniel.beatmetergenerator.editor.BeatEditor.hostServices
import org.w3c.dom.events.EventTarget
import org.w3c.dom.html.HTMLAnchorElement
import scalafx.Includes._
import scalafx.application.HostServices
import scalafx.concurrent.Worker
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.text._
import scalafx.scene.web.WebView

class LicenseDialog(hostServices: HostServices) extends Dialog {
  title = "Beatmeter Generator"
  resizable = true
  width = 400
  height = 500
  dialogPane = new DialogPane {
    headerText = "License"
    val wv = new WebView()
    content = wv
    wv.engine.getLoadWorker.state.onChange { (_, _, s) =>
      if (Worker.State.Succeeded == s) {
        val nodes = wv.engine.document.getElementsByTagName("a")
        for (i <- 0 until nodes.getLength) {
          nodes.item(i).asInstanceOf[EventTarget].addEventListener("click", e => {
            hostServices.showDocument(e.getCurrentTarget.asInstanceOf[HTMLAnchorElement].getHref)
            e.preventDefault()
          }, false)
        }
        buttonTypes = Seq(ButtonType.OK)
      }
    }
    wv.engine.load(BeatEditor.getClass.getResource("/gpl-3.0-standalone.html").toString)
    wv.contextMenuEnabled = false
  }
}