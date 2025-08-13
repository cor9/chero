name         := "Beatmeter Generator"
version      := "0.3.1"
scalaVersion := "2.13.1"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xcheckinit", "-encoding", "utf8")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.scalafx" %% "scalafx" % "12.0.2-R18",
  "org.apache.xmlgraphics" % "batik-transcoder" % "1.9",
  "org.apache.xmlgraphics" % "batik-svg-dom" % "1.9",
  "commons-io" % "commons-io" % "2.5",
  "com.lihaoyi" %% "upickle" % "0.9.0",
  "de.sciss" % "jwave" % "1.0.3"        // for jwave.Transform, Haar1, Daubechies4, etc.
)

lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
libraryDependencies ++= Seq("linux", "mac", "win").flatMap { os =>
  javaFXModules.map { m =>
    "org.openjfx" % s"javafx-$m" % "11" classifier os
  }
}

// Bring back the local LWBD sources (needed for v4lk.lwbd.* imports)
unmanagedSourceDirectories in Compile += baseDirectory.value / "lib" / "LWBD"

// You don’t need unmanaged JWave sources anymore (we pull it from Maven)

// Generate Information.scala at compile-time (UI About dialog expects it)
val maintainer = "Sklave Daniel"

sourceGenerators in Compile += Def.task {
  val out = (sourceManaged in Compile).value / "information" / "io" / "gitlab" / "sklavedaniel" / "beatmetergenerator" / "Information.scala"
  IO.write(out,
    s"""package io.gitlab.sklavedaniel.beatmetergenerator
       |
       |object Information {
       |  val version    = "${version.value}"
       |  val maintainer = "${maintainer}"
       |  val email      = "dtspam@gmx.net"
       |  val website    = "https://github.com/cor9/chero"
       |}
       |""".stripMargin)
  Seq(out)
}.taskValue

fork := true

// Keep main class so `sbt run` works
Compile / run / mainClass := Some("io.gitlab.sklavedaniel.beatmetergenerator.editor.BeatEditor")
Compile / packageBin / packageOptions += Package.ManifestAttributes("SplashScreen-Image" -> "splash.png")
