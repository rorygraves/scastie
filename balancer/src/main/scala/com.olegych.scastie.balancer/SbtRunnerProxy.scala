package com.olegych.scastie.balancer

import akka.actor.{Actor, Props}
import akka.remote.DisassociatedEvent
import akka.remote.transport.AssociationHandle.Disassociated
import com.olegych.scastie.api.Request
import com.olegych.scastie.backendapi.{ SbtPing, SbtPong}
import org.slf4j.LoggerFactory

import scala.concurrent.duration._


object SbtRunnerProxy {

  def props(runnerId: RunnerId, hostname: String, port: Int): Props =
    Props(new SbtRunnerProxy(runnerId, hostname, port))
}

/**
  * Each SbtRunnerProxy is responsible for managing the connection state of one remote sbt runner instance.
  * It will manage pings and lifecycle as well as forwarding messages back and forth.
  */
class SbtRunnerProxy private (runnerId: RunnerId, hostname: String, port: Int) extends Actor {

  private val log = LoggerFactory.getLogger(s"SbtRunnerProxy_${runnerId.id}")
  log.info(s"Started for target: $hostname:$port")

  import context.dispatcher
  private val selection = context.actorSelection(
    s"akka.tcp://SbtRemote@$hostname:$port/user/SbtActor"
  )

  context.system.scheduler.schedule(0.seconds, 1.seconds) {
    selection ! SbtPing
  }

  override def preStart: Unit = {
    context.system.eventStream.subscribe(self, classOf[DisassociatedEvent])
    ()
  }

  def disconnectedReceive: Receive = {
    case req: Request =>
      log.error("Got request whilst in disconnected state")
    case SbtPong =>
      context.parent ! ProxyConnected(runnerId)
      log.info("SbtRunner is active")
      context.become(connectedReceive, discardOld = true)
    case d: Disassociated =>
    // no nothing - we are already disconnected
    case d: DisassociatedEvent =>
    // no nothing - we are already disconnected
    case other =>
      log.info(s"Unknown message received $other")
  }

  def connectedReceive: Receive = {
    case req: Request =>
      log.info(s"Got request = forwarding to $selection")
      selection.forward(req)
    case SbtPong =>
      // do nothing
    case event: DisassociatedEvent =>
      for {
        host <- event.remoteAddress.host
        port <- event.remoteAddress.port
        if host == hostname && port == this.port
      } {
        log.warn("remote disconnected, notifying dispatcher")
        context.parent ! ProxyDisconnected(runnerId)
        context.become(disconnectedReceive, discardOld = true)
      }
    case other =>
      log.info(s"Unknown message received $other in state connectedReceive")



  }

  override def receive: Receive = disconnectedReceive
}
