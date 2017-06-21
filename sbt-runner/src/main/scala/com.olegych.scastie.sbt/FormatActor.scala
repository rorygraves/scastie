package com.olegych.scastie.sbt

import com.olegych.scastie.api.{FormatRequest, FormatResponse, ScalaTargetType}
import akka.actor.{Actor, Props}
import org.scalafmt.{Formatted, Scalafmt}
import org.scalafmt.config.{ScalafmtConfig, ScalafmtRunner}
import org.slf4j.LoggerFactory
import java.io.{PrintWriter, StringWriter}

object FormatActor {
  def props = Props(new FormatActor)
}

class FormatActor private extends Actor {
  private val log = LoggerFactory.getLogger(getClass)

  private def format(code: String,
                     worksheetMode: Boolean,
                     targetType: ScalaTargetType): Either[String, String] = {
    log.info(s"format (worksheetMode: $worksheetMode)")
    log.info(code)

    val config =
      if (worksheetMode && targetType != ScalaTargetType.Dotty)
        ScalafmtConfig.default.copy(runner = ScalafmtRunner.sbt)
      else
        ScalafmtConfig.default

    Scalafmt.format(code, style = config) match {
      case Formatted.Success(formattedCode) => Right(formattedCode)
      case Formatted.Failure(failure) =>
        val errors = new StringWriter()
        failure.printStackTrace(new PrintWriter(errors))
        val fullStack = errors.toString
        Left(fullStack)
    }
  }

  def receive: Receive = {
    case FormatRequest(code, worksheetMode, targetType) =>
      sender ! FormatResponse(format(code, worksheetMode, targetType))
  }
}
