name := "Beatmete Generator"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.1"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xcheckinit", "-encoding", "utf8")

libraryDependencies ++= Seq(
  "org.scalafx" %% "scalafx" % "8.0.102-R11",
  "org.apache.xmlgraphics" % "batik-transcoder" % "1.9",
  "org.apache.xmlgraphics" % "batik-svg-dom" % "1.9",
  "commons-io" % "commons-io" % "2.5",
  "org.rogach" %% "scallop" % "2.1.3"
)

fork := true

