package com.olegych.scastie
package web

import api._
import balancer._

import akka.pattern.ask
import akka.actor.ActorRef
import akka.util.Timeout
import akka.http.scaladsl.model.RemoteAddress

import scala.concurrent.{Future, ExecutionContext}

class RestApiServer(
    dispatchActor: ActorRef,
    ip: RemoteAddress,
    maybeUser: Option[User]
)(implicit timeout: Timeout, executionContext: ExecutionContext)
    extends RestApi {

  private def wrap(inputs: Inputs): InputsWithIpAndUser =
    InputsWithIpAndUser(inputs, UserTrace(ip.toString, maybeUser))

  private def wrapEnsime(request: EnsimeRequest): EnsimeRequestEnvelop =
    EnsimeRequestEnvelop(request, UserTrace(ip.toString, maybeUser))

  def run(inputs: Inputs): Future[SnippetId] = {
    (dispatchActor ? RunSnippet(wrap(inputs))).mapTo[SnippetId]
  }

  def format(formatRequest: FormatRequest): Future[FormatResponse] = {
    (dispatchActor ? formatRequest).mapTo[FormatResponse]
  }

  def autocomplete(
      request: AutoCompletionRequest
  ): Future[Option[AutoCompletionResponse]] = {
    (dispatchActor ? wrapEnsime(request))
      .mapTo[Option[AutoCompletionResponse]]
  }

  def typeAt(request: TypeAtPointRequest): Future[Option[TypeAtPointResponse]] = {
    (dispatchActor ? wrapEnsime(request))
      .mapTo[Option[TypeAtPointResponse]]
  }

  def updateEnsimeConfig(
      updateEnsimeConfigRequest: UpdateEnsimeConfigRequest
  ): Future[Option[EnsimeConfigUpdated]] = {
    (dispatchActor ? wrapEnsime(updateEnsimeConfigRequest))
      .mapTo[Option[EnsimeConfigUpdated]]
  }

  def save(inputs: Inputs): Future[SnippetId] = {
    (dispatchActor ? SaveSnippet(wrap(inputs))).mapTo[SnippetId]
  }

  def amend(editInputs: EditInputs): Future[Boolean] = {
    import editInputs._
    if (snippetId.isOwnedBy(maybeUser)) {
      (dispatchActor ? AmendSnippet(snippetId, wrap(inputs))).mapTo[Boolean]
    } else {
      Future.successful(false)
    }
  }

  def update(editInputs: EditInputs): Future[Option[SnippetId]] = {
    import editInputs._
    if (snippetId.isOwnedBy(maybeUser)) {
      (dispatchActor ? UpdateSnippet(snippetId, wrap(inputs)))
        .mapTo[Option[SnippetId]]
    } else {
      Future.successful(None)
    }
  }

  def delete(snippetId: SnippetId): Future[Boolean] = {
    if (snippetId.isOwnedBy(maybeUser)) {
      (dispatchActor ? DeleteSnippet(snippetId)).mapTo[Unit].map(_ => true)
    } else {
      Future.successful(false)
    }
  }

  def fork(editInputs: EditInputs): Future[Option[SnippetId]] = {
    import editInputs._
    (dispatchActor ? ForkSnippet(snippetId, wrap(inputs)))
      .mapTo[Option[SnippetId]]
  }

  def fetch(snippetId: SnippetId): Future[Option[FetchResult]] = {
    (dispatchActor ? FetchSnippet(snippetId)).mapTo[Option[FetchResult]]
  }

  def fetchOld(id: Int): Future[Option[FetchResult]] = {
    (dispatchActor ? FetchOldSnippet(id)).mapTo[Option[FetchResult]]
  }

  def fetchUser(): Future[Option[User]] =
    Future.successful(maybeUser)

  def fetchUserSnippets(): Future[List[SnippetSummary]] = {
    maybeUser match {
      case Some(user) =>
        (dispatchActor ? FetchUserSnippets(user)).mapTo[List[SnippetSummary]]
      case _ => Future.successful(Nil)
    }
  }
}
