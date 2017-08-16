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
package io.gitlab.sklavedaniel.beatmetergenerator.utils

import scalafx.scene.control.Spinner
import scalafx.Includes._

object UIUtils {
  def editableSpinner[A](spinner: Spinner[A]): Spinner[A] = {
    spinner.editable = true

    def commitEditorText(): Unit = {
      if (spinner.editable()) {
        val text = spinner.editor().text()
        if (spinner.valueFactory() != null) {
          val converter = spinner.valueFactory().getConverter
          if (converter != null) {
            spinner.valueFactory().value = converter.fromString(text)
          }
        }
      }
    }

    spinner.focused.onChange { (_, _, nv) =>
      if (!nv) {
        commitEditorText()
      }
    }
    spinner
  }
}
