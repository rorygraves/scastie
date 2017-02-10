package com.olegych.scastie
package balancer

import api._

import akka.pattern.ask
import akka.actor.ActorRef
import akka.util.Timeout
import akka.http.scaladsl.model.RemoteAddress

import scala.concurrent.{Future, ExecutionContext}

class ApiImpl(pasteActor: ActorRef, ip: RemoteAddress)(
  implicit timeout: Timeout, executionContext: ExecutionContext) extends Api {

  def run(inputs: Inputs): Future[Ressource] = {
    (pasteActor ? InputsWithIp(inputs, ip.toIP.map(_.ip.toString).getOrElse("-no-ip-"))).mapTo[Ressource]
  }

  def save(inputs: Inputs): Future[Ressource] = run(inputs)

  def fetch(id: Int): Future[Option[FetchResult]] = {
    (pasteActor ? GetPaste(id)).mapTo[Option[FetchResult]]
  }

  def format(formatRequest: FormatRequest): Future[FormatResponse] = {
    (pasteActor ? formatRequest).mapTo[FormatResponse]
  }
}
