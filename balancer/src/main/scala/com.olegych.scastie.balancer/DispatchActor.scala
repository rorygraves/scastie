package com.olegych.scastie
package balancer

import java.nio.file._

import akka.actor.{Actor, ActorLogging, ActorRef}
import com.olegych.scastie.api._
import com.olegych.scastie.balancer.internal.Server
import com.typesafe.config.ConfigFactory

import scala.collection.immutable.Queue

case class RunnerId(id: Int)
case class ProxyConnected(runnerId: RunnerId)
case class ProxyDisconnected(runnerId: RunnerId)
case class InputsWithIpAndUser(inputs: Inputs, ip: String, user: Option[User])

case class RunSnippet(inputs: InputsWithIpAndUser)
case class SaveSnippet(inputs: InputsWithIpAndUser)
case class AmendSnippet(snippetId: SnippetId, inputs: InputsWithIpAndUser)
case class UpdateSnippet(snippetId: SnippetId, inputs: InputsWithIpAndUser)
case class DeleteSnippet(snippetId: SnippetId)
case class DownloadSnippet(snippetId: SnippetId)

case class ForkSnippet(snippetId: SnippetId, inputs: InputsWithIpAndUser)

case class FetchSnippet(snippetId: SnippetId)
case class FetchOldSnippet(id: Int)
case class FetchUserSnippets(user: User)

/** Request to send the current status to the sender with the attached target information.
  * N.b. the 'target' is not used by the DispatchActor and only used by the sender.
  *
  * @param target Information for the sender about for this request
  */
case class RequestStatusFor(target: ActorRef)

/**
  * Response message for RequestStatusFor returning the current balancer information.
  * @param balancer The balancer infomation.
  * @param target Target (provided by RequestStatusFor
  */
case class RequestStatusForResponse(balancer: LoadBalancer[String], target: ActorRef)


class DispatchActor(progressActor: ActorRef, statusActor: ActorRef)
    extends Actor
    with ActorLogging {
  private val configuration =
    ConfigFactory.load().getConfig("com.olegych.scastie.balancer")

  private val portsStart = configuration.getInt("remote-ports-start")
  private val portsSize = configuration.getInt("remote-ports-size")
  private val host = configuration.getString("remote-hostname")

  private val ports = (0 until portsSize).map(portsStart + _)


  val runnerProxies: Map[RunnerId, ActorRef] = ports.zipWithIndex.map { case (port,idx) =>
    val id = idx + 1
    val runnerId = RunnerId(id)
    val ref = context.actorOf(SbtRunnerProxy.props(runnerId, host, port), s"SbtRunnerProxy_$id")
    runnerId -> ref
  }.toMap

  private var loadBalancer: LoadBalancer[String] = {
    val history = History(Queue.empty[Record[String]], size = 100)
    LoadBalancer(Vector.empty, history)
  }

  def refToProxy(runnerId: RunnerId): ActorRef = {
    runnerProxies(runnerId)
  }

  updateBalancer(loadBalancer)

  private val container = new SnippetsContainer(
    Paths.get(configuration.getString("snippets-dir")),
    Paths.get(configuration.getString("old-snippets-dir"))
  )

  private val portsInfo = ports.mkString("[", ", ", "]")
  log.info(s"connecting to: $host $portsInfo")

  private def updateBalancer(
      newBalancer: LoadBalancer[String]
  ): Unit = {
    log.info("updateBalancer")
    loadBalancer = newBalancer
    statusActor ! LoadBalancerUpdate(newBalancer)
    ()
  }

  private def run(inputsWithIpAndUser: InputsWithIpAndUser,
                  snippetId: SnippetId): Unit = {
    val InputsWithIpAndUser(inputs, ip, user) = inputsWithIpAndUser

    val userNameOpt = user.map(_.login)
    val taskId = Task.nextId
    val task = Task(taskId, userNameOpt, inputs.sbtConfig, Ip(ip), snippetId)
    log.info("run: taskId: {}, id: {}, ip: {} run {}", taskId.id, snippetId, ip, inputs)

    val (server: Server[String], newBalancer) =
      loadBalancer.add(task)

    updateBalancer(newBalancer)

    log.info(s"Sending $taskId to ${server.id}")
    refToProxy(server.id).tell(
      SbtTask(taskId, snippetId, inputs, ip, userNameOpt, progressActor),
      self
    )
  }

  def receive: Receive = {
    case format: FormatRequest =>
      val server = loadBalancer.getRandomServer
      refToProxy(server.id).forward(format)
      ()

    case req: EnsimeRequest =>
      refToProxy(loadBalancer.getRandomServer.id).forward(req)

    case RunSnippet(inputsWithIpAndUser) =>
      val InputsWithIpAndUser(inputs, _, user) = inputsWithIpAndUser
      val snippetId =
        container.create(inputs, user.map(u => UserLogin(u.login)))
      run(inputsWithIpAndUser, snippetId)
      sender ! snippetId

    case SaveSnippet(inputsWithIpAndUser) =>
      val InputsWithIpAndUser(inputs, _, user) = inputsWithIpAndUser
      val snippetId = container.save(inputs, user.map(u => UserLogin(u.login)))
      run(inputsWithIpAndUser, snippetId)
      sender ! snippetId

    case AmendSnippet(snippetId, inputsWithIpAndUser) =>
      val amendSuccess = container.amend(snippetId, inputsWithIpAndUser.inputs)
      if (amendSuccess) {
        run(inputsWithIpAndUser, snippetId)
      }
      sender ! amendSuccess

    case UpdateSnippet(snippetId, inputsWithIpAndUser) =>
      val updatedSnippetId =
        container.update(snippetId, inputsWithIpAndUser.inputs)

      updatedSnippetId.foreach(
        snippetIdU => run(inputsWithIpAndUser, snippetIdU)
      )

      sender ! updatedSnippetId

    case ForkSnippet(snippetId, inputsWithIpAndUser) =>
      val InputsWithIpAndUser(inputs, _, user) = inputsWithIpAndUser

      container
        .fork(snippetId, inputs, user.map(u => UserLogin(u.login))) match {
        case Some(forkedSnippetId) =>
          sender ! Some(forkedSnippetId)
          run(inputsWithIpAndUser, forkedSnippetId)
        case None =>
          sender ! None
      }

    case DeleteSnippet(snippetId) =>
      container.delete(snippetId)
      sender ! true

    case DownloadSnippet(snippetId) =>
      sender ! container.downloadSnippet(snippetId)

    case FetchSnippet(snippetId) =>
      sender ! container.readSnippet(snippetId)

    case FetchOldSnippet(id) =>
      sender ! container.readOldSnippet(id)

    case FetchUserSnippets(user) =>
      sender ! container.listSnippets(UserLogin(user.login))

    case FetchScalaJs(snippetId) =>
      sender ! container.readScalaJs(snippetId)

    case FetchScalaSource(snippetId) =>
      sender ! container.readScalaSource(snippetId)

    case FetchScalaJsSourceMap(snippetId) =>
      sender ! container.readScalaJsSourceMap(snippetId)

    case progress: api.SnippetProgress =>
      if (progress.done) {
        progress.snippetId.foreach(
          sid => updateBalancer(loadBalancer.done(sid))
        )
      }
      container.appendOutput(progress)

    case ProxyConnected(id) =>
      updateBalancer(loadBalancer.addServer(id, Inputs.default.sbtConfig))
    case ProxyDisconnected(id) =>
      updateBalancer(loadBalancer.removeServer(id))
    case RequestStatusFor(target) =>
      println(s" status Actor asks for status info")
      sender ! RequestStatusForResponse(loadBalancer, target)
  }
}
