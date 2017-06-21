package com.olegych.scastie.api

case class TaskId(id : Int)
case class SnippetUserPart(login: String, update: Option[Int])
case class SnippetId(base64UUID: String, user: Option[SnippetUserPart]) {
  def isOwnedBy(user2: Option[User]): Boolean = {
    (user, user2) match {
      case (Some(SnippetUserPart(snippetLogin, _)),
            Some(User(userLogin, _, _))) =>
        snippetLogin == userLogin
      case _ => false
    }
  }

  def url: String = {
    this match {
      case SnippetId(uuid, None) => uuid
      case SnippetId(uuid, Some(SnippetUserPart(login, update))) =>
        s"$login/$uuid/${update.getOrElse(0)}"
    }
  }

  def scalaJsUrl(end: String): String = {
    val middle = url
    s"/${Shared.scalaJsHttpPathPrefix}/$middle/$end"
  }
}

object User {
  // low tech solution
  val admins = Set(
    "dimart",
    "Duhemm",
    "heathermiller",
    "julienrf",
    "jvican",
    "MasseGuillaume",
    "olafurpg",
    "rorygraves",
    "travissarles"
  )
}
case class User(login: String, name: Option[String], avatar_url: String) {
  def isAdmin: Boolean = User.admins.contains(login)
}

case class SnippetSummary(snippetId: SnippetId, summary: String, time: Long)

trait Request
sealed trait Response

case class FormatRequest(code: String,
                         worksheetMode: Boolean,
                         targetType: ScalaTargetType) extends Request
case class FormatResponse(formattedCode: Either[String, String]) extends Response

sealed trait EnsimeRequest extends Request
case class CompletionRequest(inputs: Inputs, position: Int) extends EnsimeRequest
case class CompletionResponse(completions: List[Completion]) extends Response
case class TypeAtPointRequest(inputs: Inputs, position: Int) extends EnsimeRequest
case class TypeAtPointResponse(typeInfo: String) extends Response

case class FetchResult(inputs: Inputs, progresses: List[SnippetProgress])

case class FetchScalaJs(snippetId: SnippetId)
case class FetchResultScalaJs(content: String)

case class FetchScalaJsSourceMap(snippetId: SnippetId)
case class FetchResultScalaJsSourceMap(content: String)

case class FetchScalaSource(snippetId: SnippetId)
case class FetchResultScalaSource(content: String)

case class ScalaDependency(
    groupId: String,
    artifact: String,
    target: ScalaTarget,
    version: String
)

case class Project(
    organization: String,
    repository: String,
    logo: Option[String] = None,
    artifacts: List[String] = Nil
)

case class Completion(
    hint: String,
    typeInfo: String
)

case class TypeInfoAt(
    token: String,
    typeInfo: String
)

// Keep websocket connection
case class KeepAlive(msg: String = "") extends AnyVal
