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

import javafx.scene.text.FontPosture

import io.gitlab.sklavedaniel.beatmetergenerator.utils.UIUtils

import scalafx.Includes._
import scalafx.beans.binding.Bindings
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, Priority}
import scalafx.scene.text.{Font, FontWeight}
import scalafx.stage.Window

class FontDialog(ownerWindow: Option[Window], initFont: (String, Int, Boolean, Boolean)) extends Dialog[(String, Int, Boolean, Boolean)] {
  self =>
  title = "Beatmeter Generator"
  resizable = true
  width = 400
  height = 500
  ownerWindow.foreach(initOwner)
  val families = new ListView[String](Font.families.sorted) {
    cellFactory = _ => {
      val cell = new ListCell[String]() {
        item.onChange { (_, _, family) =>
          font = Font.font(family)
          text = family
        }
      }

      cell
    }
  }

  families.selectionModel().select(initFont._1)
  families.scrollTo(initFont._1)
  val size = UIUtils.editableSpinner(new Spinner[Int](5, 5000, initFont._2, 1) {
    hgrow = Priority.Always
    maxWidth = Double.PositiveInfinity
  })
  val bold = new CheckBox("Bold") {
    selected = initFont._3
  }
  val italic = new CheckBox("Italic") {
    selected = initFont._4
  }
  dialogPane = new DialogPane {
    minWidth = 200
    minHeight = 400
    headerText = "Select Font"

    val preview = new TextField {
      text = "Example"
      font <== Bindings.createObjectBinding(() => Font(families.selectionModel().getSelectedItem, if (bold.selected()) {
        FontWeight.Bold
      } else {
        FontWeight.Normal
      }, if (italic.selected()) {
        FontPosture.ITALIC
      } else {
        FontPosture.REGULAR
      }, size.value()).delegate, families.selectionModel().selectedItemProperty(), size.value, bold.selected, italic.selected)
    }
    content = new GridPane {
      hgap = 5
      vgap = 5
      add(families, 0, 0, 1, 4)
      add(size, 1, 0, 1, 1)
      add(bold, 1, 1, 1, 1)
      add(italic, 1, 2, 1, 1)
      add(preview, 0, 4, 2, 1)
    }
    buttonTypes = Seq(ButtonType.Cancel, ButtonType.OK)
  }
  resultConverter = b => {
    if (ButtonType.OK == b) {
      (families.selectionModel().getSelectedItem, size.value(), bold.selected(), italic.selected())
    } else {
      initFont
    }
  }
}

