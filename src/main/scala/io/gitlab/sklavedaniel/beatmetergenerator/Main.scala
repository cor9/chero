package io.gitlab.sklavedaniel.beatmetergenerator

import org.rogach.scallop.ScallopConf

object Main extends App {

  val beatEditorConf = new BeatEditor.Conf()

  object Conf extends ScallopConf(args) {
    addSubcommand(beatEditorConf)
    version("Beatmeter Generator 0.1.0")
    verify()
  }

  Conf.subcommand match {
    case Some(c) if c == beatEditorConf =>
      new BeatEditor(beatEditorConf).main(args)
    case _ => Conf.printHelp()
  }

}
