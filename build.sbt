import java.io.{FileOutputStream, FileWriter}
import java.nio.file.Files
import java.time.format.DateTimeFormatter.ISO_ZONED_DATE_TIME
import java.time.{ZoneId, ZonedDateTime}

import sbt.Keys.{libraryDependencies, logLevel}
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

import scala.sys.process._
import scala.util.Try

name := "scanimation-project"
scalaVersion in ThisBuild := "2.12.8"

lazy val macros = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("macros"))
  .settings(
    name := "macros",
    version := "0.1",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.8",
    test in assembly := {}
  )
  .jvmSettings(name := "macros-jvm")
  .jsSettings(name := "macros-js")

lazy val macrosJS = macros.js
lazy val macrosJVM = macros.jvm

lazy val scanimation = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(
    name := "scanimation",
    version := "0.1",
    test in assembly := {},
    logLevel := Level.Error
  )
  .jvmSettings(
    name := "jvm",

    // assembly
    test in assembly := {},
    mainClass in assembly := Some("scanimation.launcher"),
    assemblyJarName in assembly := "scanimation.jar",

    // docker
    test in docker := {},
    dockerfile in docker := {
      val artifact: File = assembly.value
      val artifactTargetPath = s"/app/${artifact.name}"

      new Dockerfile {
        from("openjdk:8-jre")
        add(artifact, artifactTargetPath)
        cmdRaw(s"java $$JVM_CONFIG -jar -Dgeneral.port=$$PORT $artifactTargetPath")
      }
    },
    imageNames in docker := Seq(ImageName(s"wispy/scanimation:latest"), ImageName(s"registry.heroku.com/wispy-scanimation/web")),

    resolvers += Resolver.jcenterRepo,

    // logging
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
    // akka
    libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.22",
    libraryDependencies += "com.typesafe.akka" %% "akka-http" % "10.1.8",
    libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.5.22",
    libraryDependencies += "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.8",
    libraryDependencies += "ch.megard" %% "akka-http-cors" % "0.4.0",
    // spray
    libraryDependencies += "io.spray" %% "spray-json" % "1.3.5",
    // discord client
    libraryDependencies += "net.dv8tion" % "JDA" % "3.8.3_463",
    // mongo
    libraryDependencies += "org.mongodb.scala" %% "mongo-scala-driver" % "2.6.0",
    libraryDependencies += "com.github.simplyscala" %% "scalatest-embedmongo" % "0.2.4" % Test,
    // image processing
    libraryDependencies += "net.coobird" % "thumbnailator" % "0.4.8",
    // akka
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.5.22" % Test,
    libraryDependencies += "com.typesafe.akka" %% "akka-http-testkit" % "10.1.8" % Test,
    libraryDependencies += "com.typesafe.akka" %% "akka-stream-testkit" % "2.5.22" % Test
  )
  .jsSettings(
    name := "js",
    test in assembly := {},
    // calls main method when js file is loaded
    scalaJSUseMainModuleInitializer := true,
    // dom - basic dom operations lib
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.2",
    // jquery
    libraryDependencies += "org.querki" %%% "jquery-facade" % "1.2",
  )
  .dependsOn(macros)

lazy val scanimationJVM = scanimation.jvm.enablePlugins(DockerPlugin)
lazy val scanimationJS = scanimation.js

lazy val moveJS = taskKey[Unit]("moveJS")
lazy val pushJS = taskKey[Unit]("pushJS")
lazy val nodeJS = taskKey[Unit]("nodeJS")
lazy val lessJS = taskKey[Unit]("lessJS")
lazy val deployHeroku = taskKey[Unit]("deployHeroku")

def copyFile(from: String, to: String): Unit = {
  val in = new File(from).toPath
  val out = new File(to)
  out.delete()
  println(s"[scanimation] Copying [${in.toAbsolutePath}] to [${out.getAbsolutePath}]")
  out.createNewFile()
  val stream = new FileOutputStream(out)
  Files.copy(in, stream)
  Try(stream.close())
}

def moveFile(from: String, to: String): Unit = {
  copyFile(from, to)
  new File(from).delete()
}

def fileExists(path: String): Boolean = {
  new File(path).isFile
}

def copyFolder(from: String, to: String): Unit = {
  new File(to).mkdirs()
  Option(new File(from).listFiles())
    .getOrElse(Array())
    .foreach {
      case folder if folder.isDirectory => copyFolder(s"$from/${folder.getName}", s"$to/${folder.getName}")
      case file => copyFile(s"$from/${file.getName}", s"$to/${file.getName}")
    }
}

def writeFile(file: String, content: String): Unit = {
  val out = new File(file)
  val writer = new FileWriter(out)
  writer.write(content)
  writer.flush()
  Try(writer.close())
}

def execute(commands: String*): Int = {
  s"cmd /C ${commands.mkString(" & ")}".!
}

nodeJS := {
  println("[scanimation] Building node modules into script file")
  execute(
    """cd ./node""",
    """npm install""",
    """npm install --global browserify""",
    """browserify main.js -o bundle.js"""
  )
}

moveJS := {
  println("[scanimation] Moving javascript files to output folder")
  if (fileExists("./js/target/scala-2.12/js-opt.js")) {
    moveFile("./js/target/scala-2.12/js-opt.js", "./out/scanimation.js")
    moveFile("./js/target/scala-2.12/js-opt.js.map", "./out/scanimation.js.map")
  } else {
    moveFile("./js/target/scala-2.12/js-fastopt.js", "./out/scanimation.js")
    moveFile("./js/target/scala-2.12/js-fastopt.js.map", "./out/scanimation.js.map")
  }
  moveFile("./node/bundle.js", "./out/bundle.js")
}

pushJS := {
  println("[scanimation] Pushing javascript output to github.io repository")
  writeFile("./out/timestamp.txt", ZonedDateTime.now(ZoneId.of("UTC")).format(ISO_ZONED_DATE_TIME))
  copyFolder("./out", "../scanimation.github.io")
  execute(
    """cd ../scanimation.github.io""",
    """git pull""",
    """git add .""",
    """git commit -m "js deployment"""",
    """git push origin master --recurse-submodules=check"""
  )
}

deployHeroku := {
  println("[scanimation] Deploying heroku jvm container")
  execute(
    """heroku container:login""",
    """docker push registry.heroku.com/wispy-scanimation/web""",
    """heroku container:release web --app wispy-scanimation"""
  )
}

lessJS := {
  println("[scanimation] Compiling LESS into CSS")
  execute(
    """cd ./out""",
    """npm install --global less""",
    """lessc styles.less styles.css"""
  )
}