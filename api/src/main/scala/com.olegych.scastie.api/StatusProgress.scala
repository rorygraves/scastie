package com.olegych.scastie
package api

sealed trait StatusProgress
case object StatusKeepAlive extends StatusProgress
case class StatusInfo(runners: Vector[RunnerStatus]) extends StatusProgress

case class RunnerStatus(tasks: List[TaskStatus])


case class TaskStatus(taskId: TaskId, user: Option[String], snippetId: SnippetId)
