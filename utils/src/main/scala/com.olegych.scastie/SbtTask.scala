package com.olegych.scastie

import api._

import akka.actor.ActorRef

case class SbtTask(taskId: TaskId,
                   snippetId: SnippetId,
                   inputs: Inputs,
                   ip: String,
                   login: Option[String],
                   progressActor: ActorRef) extends Request
