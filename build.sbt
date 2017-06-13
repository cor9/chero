name := "Beatmeter Generator"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.2"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xcheckinit", "-encoding", "utf8")

libraryDependencies ++= Seq(
  "org.scalafx" %% "scalafx" % "8.0.102-R11",
  "org.apache.xmlgraphics" % "batik-transcoder" % "1.9",
  "org.apache.xmlgraphics" % "batik-svg-dom" % "1.9",
  "commons-io" % "commons-io" % "2.5",
  "org.rogach" %% "scallop" % "2.1.3"
)

fork := true

mainClass in assembly := Some("io.gitlab.sklavedaniel.beatmetergenerator.Main")

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}

assemblyJarName in assembly := s"beatmeter-generator.jar"