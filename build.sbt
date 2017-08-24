name := "Beatmeter Generator"

version := "0.2.1"

scalaVersion := "2.12.2"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xcheckinit", "-encoding", "utf8")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.scalafx" %% "scalafx" % "8.0.102-R11",
  "org.apache.xmlgraphics" % "batik-transcoder" % "1.9",
  "org.apache.xmlgraphics" % "batik-svg-dom" % "1.9",
  "commons-io" % "commons-io" % "2.5",
  "org.rogach" %% "scallop" % "2.1.3",
  "com.jsuereth" %% "scala-arm" % "2.0",
  "org.scala-lang" % "scala-reflect" % "2.12.2",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.github.benhutchison" %% "prickle" % "1.1.13"
)

unmanagedSourceDirectories in Compile += baseDirectory.value / "lib/JWave/src"


fork := true

javaOptions := Seq("-splash:src/main/resources/splash.png")

mainClass in assembly := Some("io.gitlab.sklavedaniel.beatmetergenerator.editor.BeatEditor")
mainClass in Compile := Some("io.gitlab.sklavedaniel.beatmetergenerator.editor.BeatEditor")
mainClass in run := Some("io.gitlab.sklavedaniel.beatmetergenerator.editor.BeatEditor")
assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs@_*) => MergeStrategy.discard
  case x => MergeStrategy.first
}

assemblyJarName in assembly := s"beatmeter-generator.jar"

packageOptions in (Compile, packageBin) +=
  Package.ManifestAttributes("SplashScreen-Image" -> "splash.png")

packageOptions in assembly +=
  Package.ManifestAttributes("SplashScreen-Image" -> "splash.png")


cancelable in Global := true

enablePlugins(JDKPackagerPlugin)

jdkPackagerType := "msi"

packageOptions in packageJavaLauncherJar +=
  Package.ManifestAttributes("SplashScreen-Image" -> "splash.png")

mappings in packageJavaLauncherJar += (baseDirectory.value / "src/main/resources/splash.png" -> "splash.png")

maintainer := "Sklave Daniel"

packageSummary := "Editor and generator for beat patterns and beat indicators for music videos."

packageDescription := "Editor and generator for beat patterns and beat indicators for music videos."

jdkPackagerProperties := Map()

jdkPackagerJVMArgs := Seq("-Xmx1g")

jdkPackagerAppArgs := Seq()

lazy val osext = sys.props("os.name").toLowerCase match {
  case os if os.contains("mac") ⇒ "icns"
  case os if os.contains("win") ⇒ "ico"
  case _ ⇒ "png"
}

jdkAppIcon :=  Some(sourceDirectory.value / "main" / "resources" / "icon" / s"icon-128x128.$osext")


sourceGenerators in Compile += Def.task {
  val file = (sourceManaged in Compile).value / "information" / "io" / "gitlab" / "sklavedaniel" / "beatmetergenerator" / "Information.scala"
  IO.write(file,
    s"""package io.gitlab.sklavedaniel.beatmetergenerator
       |
       | object Information {
       |   val version = "${version.value}"
       |   val maintainer = "${maintainer.value}"
       |   val email = "dtspam@gmx.net"
       |   val website = "https://gitlab.com/SklaveDaniel/BeatmeterGenerator"
       | }""".stripMargin)
  Seq(file)
}.taskValue

