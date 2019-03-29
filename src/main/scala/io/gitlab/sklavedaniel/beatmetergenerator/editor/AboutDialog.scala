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
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.text._
import scalafx.Includes._
import scalafx.application.HostServices

class AboutDialog(hostServices: HostServices) extends Dialog {
  title = "Beatmeter Generator"
  resizable = true
  width = 200
  height = 150
  dialogPane = new DialogPane {
    headerText = "About"
    content = new GridPane {
      hgap = 10
      vgap = 5
      add(new Text("Version"), 0, 0)
      add(new Text(Information.version), 1, 0)
      add(new Text("Maintainer"), 0, 1)
      add(new Text(Information.maintainer), 1, 1)
      add(new Text("E-Mail"), 0, 2)
      add(new Hyperlink(Information.email) {
        onAction = handle {
          hostServices.showDocument("mailto:" + Information.email)
        }
      }, 1, 2)
      add(new Text("Website"), 0, 3)
      add(new Hyperlink(Information.website) {
        onAction = handle {
          hostServices.showDocument(Information.website)
        }
      }, 1, 3)
    }
    buttonTypes = Seq(ButtonType.OK)
  }
}