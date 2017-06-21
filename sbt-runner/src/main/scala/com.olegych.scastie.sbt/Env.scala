package com.olegych.scastie.sbt

/**
  * Environment defined parameters
  */
object Env {
  val ensimeUser: Option[String] = Option(System.getenv("ENSIME_USER_GROUP")).filter(_.nonEmpty)
}
