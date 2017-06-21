package com.olegych.scastie.sbt

import akka.actor.{Actor, ActorSystem}
import com.olegych.scastie.api._
import com.olegych.scastie.backendapi.{SbtPing, SbtPong}
import com.olegych.scastie.{NoUserAndGroup, SbtTask, SomeUserAndGroup}
import org.slf4j.LoggerFactory

import scala.concurrent.duration._

class SbtActor(system: ActorSystem,
               runTimeout: FiniteDuration,
               production: Boolean,
               withEnsime: Boolean)
    extends Actor {

  private val log = LoggerFactory.getLogger(getClass)

  private val formatActor =
    context.actorOf(FormatActor.props, name = "FormatActor")

  private val sbtRunner =
    context.actorOf(
      SbtRunner.props(runTimeout, production),
      name = "SbtRunner"
    )

  private val ensimeActor =
    if (withEnsime) {

      val ensimeUserAndGroup = Env.ensimeUser match {
        case Some(user) =>
          SomeUserAndGroup(user,user)
        case None => NoUserAndGroup
      }

      log.info(s"Ensime UserGroup = $ensimeUserAndGroup")
      Some(
        context.actorOf(
          EnsimeActor.props(system, sbtRunner, ensimeUserAndGroup),
          name = "EnsimeActor"
        )
      )
    } else {
      None
    }

  def receive: Receive = {
    case SbtPing =>
      sender ! SbtPong

    case req: EnsimeRequest =>
      ensimeActor.foreach(_.forward(req))

    case format: FormatRequest =>
      formatActor.forward(format)

    case task: SbtTask =>
      log.info(s"Received task ${task.taskId} sending to runner")
      sbtRunner.forward(task)
  }
}
