name := "Beatmeter Generator"

version := "0.1-SNAPSHOT"

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

mainClass in assembly := Some("io.gitlab.sklavedaniel.beatmetergenerator.editor.BeatEditor")
mainClass in (Compile, run) := Some("io.gitlab.sklavedaniel.beatmetergenerator.editor.BeatEditor")

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}

assemblyJarName in assembly := s"beatmeter-generator.jar"

cancelable in Global := true
