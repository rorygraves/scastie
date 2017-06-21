package com.olegych.scastie
package balancer

import api._
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSelection, Props}
import akka.stream.actor.ActorPublisher
import akka.stream.scaladsl.Source
import akka.stream.actor.ActorPublisherMessage.Request
import java.util.concurrent.TimeUnit

import scala.collection.mutable
import scala.concurrent.duration._
import scala.collection.mutable.{Queue => MQueue}

case object SubscribeStatus
case class LoadBalancerUpdate(
    newBalancer: LoadBalancer[String]
)

case class SetDispatcher(dispatchActor: ActorRef)

class StatusActor extends Actor with ActorLogging {
  private val publishers = mutable.Buffer.empty[ActorRef]

  private var dispatchActor: Option[ActorRef] = _

  def receive: Receive = {
    case SubscribeStatus =>
      val publisher = context.actorOf(Props(new StatusForwarder))
      publishers += publisher

      val source =
        Source
          .fromPublisher(ActorPublisher[StatusProgress](publisher))
          .keepAlive(
            FiniteDuration(1, TimeUnit.SECONDS),
            () => StatusKeepAlive
          )

      sender ! source

      dispatchActor.foreach(_ ! RequestStatusFor(publisher))

    case LoadBalancerUpdate(newBalancer) =>
      val message = convert(newBalancer)
      publishers.foreach(_ ! message)

    case RequestStatusForResponse(balancer, originalRequestor) =>
      originalRequestor ! convert(balancer)

    case SetDispatcher(dispatchActorReference) =>
      dispatchActor = Some(dispatchActorReference)
  }

  private def convert(
      newBalancer: LoadBalancer[String]
  ): StatusProgress = {
    StatusInfo(
      newBalancer.servers.map(
        server => RunnerStatus(server.mailbox.map(task => TaskStatus(task.taskId, task.user, task.snippetId)).toList)
      )
    )
  }
}

class StatusForwarder extends Actor with ActorPublisher[StatusProgress] {
  private var buffer = MQueue.empty[StatusProgress]
  private val maxSize = 10

  def receive: Receive = {
    case progress: StatusProgress =>
      if (buffer.size >= maxSize) {
        buffer.dequeue()
      }
      buffer.enqueue(progress)
      deliver()
    case _: Request =>
      deliver()
  }

  private def deliver(): Unit = {
    if (totalDemand > 0) {
      buffer.foreach { progress =>
        onNext(progress)
      }
      buffer.clear()
    }
  }
}
