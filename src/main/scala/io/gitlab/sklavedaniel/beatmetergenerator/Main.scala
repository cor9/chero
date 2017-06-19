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

package io.gitlab.sklavedaniel.beatmetergenerator

import org.rogach.scallop.{ScallopConf, Subcommand}

object Main extends App {

  abstract class ExecutableSubcommand(name: String) extends Subcommand(name) {
    def execute(args: Array[String])
  }

  val commands: Seq[ExecutableSubcommand] = Seq(
    new BeatEditor.Conf(),
    new AudioGenerator.Conf(),
    new VideoGenerator.Conf()
  )

  object Conf extends ScallopConf(args) {
    for (subcommand: ExecutableSubcommand <- commands)
      addSubcommand(subcommand)
    version("Beatmeter Generator 0.1.0")
    verify()
  }

  Conf.subcommand match {
    case Some(c: ExecutableSubcommand) =>
      c.execute(args)
    case _ => Conf.printHelp()
  }

}
