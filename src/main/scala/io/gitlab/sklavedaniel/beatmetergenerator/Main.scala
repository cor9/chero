package io.gitlab.sklavedaniel.beatmetergenerator

import java.io.{BufferedInputStream, File, FileInputStream}

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
