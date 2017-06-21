package com.olegych.scastie.sbt

import com.olegych.scastie.api._

import scala.util.Random
import System.{lineSeparator => nl}

import org.slf4j.LoggerFactory
import java.io.{BufferedReader, IOException, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.nio.file.Path

import com.olegych.scastie.{FileUtil, NoUserAndGroup, SomeUserAndGroup, UserAndGroup}

class Sbt(defaultConfig: Inputs, name: String)
         (implicit val userAndGroup: UserAndGroup)
{
  val sbtDir: Path = FileUtil.createTempDir("scastie")

  FileUtil.setFileOwnership(sbtDir, userAndGroup)

  private val log = LoggerFactory.getLogger(getClass.toString + "(" + name + ")")
  private val sbtLog = LoggerFactory.getLogger(s"SbtOutputIO($name)")

  log.info("Starting")
  log.info(s"sbtDir set to $sbtDir")

  private val uniqueId = Random.alphanumeric.take(10).mkString

  private var currentSbtConfig = ""
  private var currentSbtPluginsConfig = ""

  private val buildFile = sbtDir.resolve("build.sbt")
  private val prompt = s"""shellPrompt := (_ => "$uniqueId\\n")"""

  private val projectDir = sbtDir.resolve("project")
  FileUtil.createDirectories(projectDir)

  val startScript: Path = sbtDir.resolve("startSbt.sh")
  FileUtil.write(startScript,s"cd ${sbtDir.toAbsolutePath}\nexport CI=true\nsbt")
  FileUtil.makeExecutable(startScript)
  FileUtil.write(projectDir.resolve("build.properties"), s"sbt.version = 0.13.15")

  private val pluginFile = projectDir.resolve("plugins.sbt")

  private val ensimeVersion = "2.0.0-SNAPSHOT"
  private val secretSbtConfigExtra = s"""
                                        |// this is where the ensime-server snapshots are hosted
                                        |resolvers += Resolver.sonatypeRepo("snapshots")
                                        |libraryDependencies += "org.ensime" %% "ensime" % "$ensimeVersion"
                                        |""".stripMargin

  private def setup(): Unit = {
    setConfig(defaultConfig)
    setPlugins(defaultConfig)
  }

  log.info("Running setup")
  setup()
  log.info("Setup complete")

  val codeFile: Path = sbtDir.resolve("src/main/scala/main.scala")
  FileUtil.createDirectories(codeFile.getParent)

  def scalaJsContent(): Option[String] = {
    FileUtil.slurp(sbtDir.resolve(ScalaTarget.Js.targetFilename))
  }

  def scalaJsSourceMapContent(): Option[String] = {
    FileUtil.slurp(sbtDir.resolve(ScalaTarget.Js.sourceMapFilename))
  }

  private val (process, fin, ferr, fout) = {
    val command = userAndGroup match {
      case SomeUserAndGroup(userName, _) =>
        List("sudo", "-i", "-u", userName, "--", s"${sbtDir.toFile}/startSbt.sh")
      case NoUserAndGroup =>
        List("sbt")
    }

    log.info(s"Starting sbt process: ${command.mkString(" ")} in ${sbtDir.toFile}")
    val builder = new ProcessBuilder(command: _*).directory(sbtDir.toFile)
    val buildEnv = builder.environment()
    buildEnv.put(
        "SBT_OPTS",
        Seq(
          "-Xms512m",
          "-Xmx1g",
          "-Djline.terminal=jline.UnsupportedTerminal",
          "-Dsbt.log.noformat=true"
        ).mkString(" ")
      )
    buildEnv.put("CI", "true")
    buildEnv.put("COURSIER_PROGRESS", "false")


    val process = builder.start()

    val in = new BufferedReader(
      new InputStreamReader(process.getInputStream, StandardCharsets.UTF_8)
    )

    val err = new BufferedReader(
      new InputStreamReader(process.getErrorStream, StandardCharsets.UTF_8)
    )

    log.info("sbt process running")
    (process, process.getOutputStream, err, in)
  }

  private def collect(
                       lineCallback: (String, Boolean, Boolean, Boolean) => Unit,
                       reload: Boolean
                     ): Boolean = {
    val inChars = new collection.mutable.Queue[Character]()
    val errorChars = new collection.mutable.Queue[Character]()
    var read = 0
    var done = false
    var sbtError = false
    log.info("Running collect")
    while(!done) {
      val foutReady = fout.ready()
      val ferrReady = ferr.ready()
      if(!foutReady && ! ferrReady)
        Thread.sleep(10) // to stop thrashing

      if(foutReady) {
        read = fout.read()
        if (read == 10) {
          val line = inChars.mkString

          val prompt = line == uniqueId
          sbtError = line == "[error] Type error in expression"
          done = prompt || sbtError

          sbtLog.info(s"OUT: $line")

          lineCallback(line, done, sbtError, reload)
          inChars.clear()
        } else {
          inChars += read.toChar
        }
      }
      if(ferrReady && !done) {
        read = ferr.read()
        if (read == 10) {
          val line = errorChars.mkString

          sbtLog.info(s"ERR: $line")

//          lineCallback(line, done, sbtError, reload)
          errorChars.clear()
        } else {
          errorChars += read.toChar
        }
      }
    }
    if (sbtError) {
      log.info("Saw sbt error retriggering setup")
      setup()
      // wait for initialise to be complete
      process("r", noop, reload = false)
      log.info("reset complete")
    }

    sbtError
  }

  type LineCallback = (String, Boolean, Boolean, Boolean) => Unit
  val noop: LineCallback = (_, _, _, _) => ()

  collect(noop, reload = false)
  log.info("sbt initialised")

  private def process(command: String,
                      lineCallback: LineCallback,
                      reload: Boolean): Boolean = {
    fin.write((command + nl).getBytes)
    fin.flush()
    collect(lineCallback, reload)
  }

  def kill(): Unit = {
    val pidField = process.getClass.getDeclaredField("pid")
    pidField.setAccessible(true)
    val pid = pidField.get(process).asInstanceOf[Int]
    import sys.process._
    s"pkill -KILL -P $pid".!
    ()
  }

  def needsReload(inputs: Inputs): Boolean =
    inputs.sbtConfig != currentSbtConfig ||
      inputs.sbtPluginsConfig != currentSbtPluginsConfig

  def exit(): Unit = {
    process("exit", noop, reload = false)
    ()
  }

  private def writeFile(path: Path, content: String): Unit = {
    if (FileUtil.exists(path))
      FileUtil.delete(path)

    FileUtil.write(path, content)

    ()
  }

  private def setPlugins(inputs: Inputs): Unit = {
    writeFile(pluginFile, inputs.sbtPluginsConfig + nl)
    currentSbtPluginsConfig = inputs.sbtPluginsConfig
  }

  private def setConfig(inputs: Inputs): Unit = {
    writeFile(buildFile,
      prompt + nl + inputs.sbtConfig + nl + secretSbtConfigExtra)
    currentSbtConfig = inputs.sbtConfig
  }

  def eval(command: String,
           inputs: Inputs,
           lineCallback: LineCallback,
           reload: Boolean): Boolean = {
    maybeReloadAndEval(command = command,
      commandIfNeedsReload = "",
      inputs,
      lineCallback,
      reload)
  }

  def evalIfNeedsReload(command: String,
                        inputs: Inputs,
                        lineCallback: LineCallback,
                        reload: Boolean): Boolean = {
    maybeReloadAndEval(command = "",
      commandIfNeedsReload = command,
      inputs,
      lineCallback,
      reload)
  }

  private def maybeReloadAndEval(command: String,
                                 commandIfNeedsReload: String,
                                 inputs: Inputs,
                                 lineCallback: LineCallback,
                                 reload: Boolean) = {

    val isReloading = needsReload(inputs)
    log.info(s"maybeReloadAndEval isReloading: $isReloading")
    if (inputs.sbtConfig != currentSbtConfig) {
      setConfig(inputs)
    }

    if (inputs.sbtPluginsConfig != currentSbtPluginsConfig) {
      setPlugins(inputs)
    }

    val reloadError =
      if (isReloading) {
        process("reload", lineCallback, reload = true)
      } else false

    if (!reloadError) {
      FileUtil.write(codeFile, inputs.code, truncate = true)
      try {
        if (isReloading && !commandIfNeedsReload.isEmpty)
          process(commandIfNeedsReload, lineCallback, reload)
        if (!command.isEmpty) {
          log.info(s"Running command: $command -----------------------------------")
          val res = process(command, lineCallback, reload)
          println(" RES = " + res + "---------------------------------------------")
          res
        }
      } catch {
        case e: IOException =>
          log.error("Got IOException: ",e)
          // when the snippet is pkilled (timeout) the sbt output stream is closed
          if (e.getMessage == "Stream closed") ()
          else throw e
        case ex: Throwable =>
          log.error("Got exception: ",ex)
      }
    } else
      log.info("reload failed")

    reloadError
  }
}
