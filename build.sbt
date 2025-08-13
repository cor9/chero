libraryDependencies ++= Seq(
  "org.scalafx" %% "scalafx" % "12.0.2-R18",
  "org.apache.xmlgraphics" % "batik-transcoder" % "1.9",
  "org.apache.xmlgraphics" % "batik-svg-dom" % "1.9",
  "commons-io" % "commons-io" % "2.5",
  "com.lihaoyi" %% "upickle" % "0.9.0",
  "de.sciss" % "jwave" % "1.0.3"
)

lazy val javaFXModules = Seq(
  "base", "controls", "fxml", "graphics", "media", "swing", "web"
)

libraryDependencies ++= Seq("linux", "mac", "win").flatMap { os =>
  javaFXModules.map { m =>
    "org.openjfx" % s"javafx-$m" % "11" classifier os
  }
}
