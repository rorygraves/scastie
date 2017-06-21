package com.olegych.scastie.balancer.internal

import com.olegych.scastie.api.SnippetId
import com.olegych.scastie.balancer.{RunnerId, Task}

import scala.collection.immutable.Queue

case class Server[C](id: RunnerId, lastConfig: C, mailbox: Queue[Task[C]]) {

  def currentSnippetId: Option[SnippetId] = mailbox.headOption.map(_.snippetId)
  def currentConfig: C = mailbox.headOption.map(_.config).getOrElse(lastConfig)

  def done: Server[C] = {
    val (task, mailbox0) = mailbox.dequeue

    assert(currentSnippetId.contains(task.snippetId))

    copy(
      lastConfig = task.config,
      mailbox = mailbox0
    )
  }

  def add(task: Task[C]): Server[C] = {
    copy(mailbox = mailbox.enqueue(task))
  }

  def cost: Int = {
    import Server._

    val reloadsPenalties =
      mailbox.sliding(2).foldLeft(0) { (acc, slide) =>
        val reloadPenalty =
          slide match {
            case Queue(x, y) =>
              if (x.config != y.config) averageReloadTime
              else 0
            case _ => 0
          }
        acc + reloadPenalty
      }

    reloadsPenalties + mailbox.map(_ => averageRunTime).sum
  }
}
object Server {
  // (found by experimentation)
  val averageReloadTime = 10 //s

  //([0s, 10s] upper bound Defined in SbtMain)
  val averageRunTime = 3 // s

  def apply[C](ref: RunnerId, config: C): Server[C] =
    Server(
      id = ref,
      lastConfig = config,
      mailbox = Queue()
    )
}
