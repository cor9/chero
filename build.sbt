import java.io.{FileOutputStream, OutputStream}
import java.lang
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

name := "Beatmeter Generator"

version := "0.3.1"

scalaVersion := "2.12.8"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xcheckinit", "-encoding", "utf8")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.scalafx" %% "scalafx" % "11-R16",
  "org.apache.xmlgraphics" % "batik-transcoder" % "1.9",
  "org.apache.xmlgraphics" % "batik-svg-dom" % "1.9",
  "commons-io" % "commons-io" % "2.5",
  "com.jsuereth" %% "scala-arm" % "2.0",
  "com.github.benhutchison" %% "prickle" % "1.1.13"
)

lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
libraryDependencies ++= Seq("linux", "mac", "win").flatMap(os => javaFXModules.map( m =>
  "org.openjfx" % s"javafx-$m" % "11" classifier os
))

unmanagedSourceDirectories in Compile += baseDirectory.value / "lib/JWave/src"

fork := true

mainClass in assembly := Some("io.gitlab.sklavedaniel.beatmetergenerator.editor.BeatEditor")
mainClass in Compile := Some("io.gitlab.sklavedaniel.beatmetergenerator.editor.BeatEditor")
mainClass in run := Some("io.gitlab.sklavedaniel.beatmetergenerator.editor.BeatEditor")
assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs@_*) => MergeStrategy.discard
  case x => MergeStrategy.first
}

assemblyJarName in assembly := s"beatmeter-generator.jar"

packageOptions in(Compile, packageBin) +=
  Package.ManifestAttributes("SplashScreen-Image" -> "splash.png")

packageOptions in assembly +=
  Package.ManifestAttributes("SplashScreen-Image" -> "splash.png")

cancelable in Global := true

val maintainer = "Sklave Daniel"

sourceGenerators in Compile += Def.task {
  val file = (sourceManaged in Compile).value / "information" / "io" / "gitlab" / "sklavedaniel" / "beatmetergenerator" / "Information.scala"
  IO.write(file,
    s"""package io.gitlab.sklavedaniel.beatmetergenerator
       |
       | object Information {
       |   val version = "${version.value}"
       |   val maintainer = "${maintainer}"
       |   val email = "dtspam@gmx.net"
       |   val website = "https://gitlab.com/SklaveDaniel/BeatmeterGenerator"
       | }""".stripMargin)
  Seq(file)
}.taskValue

val packageMSI = taskKey[Unit]("Generate msi package.")

packageMSI := {
  val winjdks = List(("x64", "ProgramFiles64Folder", "25ec46a1-f39c-4b67-be83-9aeb3c67e923",
    "e4f4b924-5279-11e9-9a85-3c970ec1ef9e", new File("/root/jre-11-win-64/")))
  val ico = sourceDirectory.value / "main" / "resources" / "icon" / "icon-128x128.ico"
  val lcname = name.value.toLowerCase.replace(" ", "-")
  val icoName = s"$lcname.ico"
  val lcmaintainer = maintainer.toLowerCase.replace(" ", "-")

  val jar = assembly.value

  val output = target.value / "msiPackage"

  IO.write(output / s"$lcname.js",s"""new ActiveXObject("WScript.Shell").Run("runtime\\\\bin\\\\javaw -jar ${jar.getName}")""")

  def generate(arch: String, folder: String, productId: String, updateId: String, jdk: File) {
    def getRuntimeXML(file: File, fileStart: Int, directoryStart: Int): (String, Int
      , Int) = {
      var
      fileId = fileStart
      var
      directoryId = directoryStart
      val tmp = for (f <- file.
        listFiles().toList) yield
        if (f.isDirectory) {
          val (s, i, j) =
            getRuntimeXML(f,
              fileId, directoryId)
          fileId = i
          directoryId = j + 1
          s"""<Directory Id="dir$directoryId" Name="${f.getName}">$s</Directory>"""
        } else {
          fileId += 1
          s"""<Component Id="cmp$fileId" Guid="*"><File Id="fil$fileId" KeyPath="yes" Source="${f.getPath}"/></Component>"""
        }
      (tmp.mkString, fileId, directoryId)
    }

    val (runtimeFiles, runtimeIds, _) = getRuntimeXML(jdk, 0, 0)

    IO.write(output / s"$lcname-$arch.wxs",
      s"""<?xml version='1.0'?>
        <Wix xmlns='http://schemas.microsoft.com/wix/2006/wi'>

          <Product Name='${name.value} ${version.value} $arch' Id='$productId' UpgradeCode='$updateId'
            Language='1033' Codepage='1252' Version='${version.value}' Manufacturer='$maintainer'>

            <Package Id='*' Keywords='Installer' Description="${name.value} ${version.value} Installer"
              Manufacturer='$maintainer'
              InstallerVersion='200' Languages='1033' Compressed='yes' SummaryCodepage='1252'/>

            <Media Id='1' Cabinet='Sample.cab' EmbedCab='yes' DiskPrompt="CD-ROM #1" />
            <Property Id='DiskPrompt' Value="${name.value} ${version.value} Installation [1]" />

            <Icon Id="$lcname.ico" SourceFile="${ico.toString}" />

            <Directory Id='TARGETDIR' Name='SourceDir'>
              <Directory Id='$folder' Name='PFiles'>
                <Directory Id='$lcmaintainer' Name='$lcmaintainer'>
                  <Directory Id='INSTALLDIR' Name='${name.value}'>
                    <Component Id="starterComponent" Guid="*">
                      <File Id="starterFile" KeyPath="yes" Source="$lcname.js">
                        <Shortcut Id="desktopIcon" Directory="DesktopFolder" Name="${name.value}" WorkingDirectory='INSTALLDIR' Icon="$lcname.ico" Advertise="yes"/>
                      </File>
                    </Component>
                    <Component Id="jarComponent" Guid="*">
                      <File Id="jarFile" KeyPath="yes" Source="$jar"/>
                    </Component>
                    <Directory Id="runtimeDir" Name="runtime">
                      $runtimeFiles
                    </Directory>
                  </Directory>
                </Directory>
              </Directory>
            </Directory>

            <Feature Id='Complete' Level='1'>
              <ComponentRef Id="starterComponent"/>
              <ComponentRef Id="jarComponent"/>
              ${(1 to runtimeIds).map(i => s"""<ComponentRef Id="cmp$i"/>""").mkString("\n")}
            </Feature>
          </Product>
        </Wix>
      """)

    new lang.ProcessBuilder("wixl", "-v", "--arch", arch, s"$lcname-$arch.wxs").directory(output).inheritIO().start().waitFor()
  }

  for ((arch, folder, productId, updateId, jdk) <- winjdks) {
    generate(arch, folder, productId, updateId, jdk)
  }


}


