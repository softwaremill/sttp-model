package sttp.model.headers

import sttp.model.headers.WWWAuthenticateChallenge.{CharsetParam, RealmParam}

import scala.collection.immutable.ListMap

case class WWWAuthenticateChallenge(scheme: String, params: ListMap[String, String]) {
  override def toString: String = {
    val paramsAsString = params.map { case (k, v) => s"""$k="$v"""" }.mkString(", ")
    val sep = if (paramsAsString.nonEmpty) " " else ""
    s"$scheme$sep$paramsAsString"
  }

  def realm: Option[String] = param(RealmParam)
  def realm(r: String): WWWAuthenticateChallenge = addParam(RealmParam, r)

  def charset: Option[String] = param(CharsetParam)
  def charset(c: String): WWWAuthenticateChallenge = addParam(CharsetParam, c)

  def param(key: String): Option[String] = params.get(key)
  def addParam(key: String, value: String): WWWAuthenticateChallenge = copy(params = params + (key -> value))
}

object WWWAuthenticateChallenge {
  val RealmParam = "realm"
  val CharsetParam = "charset"
  val BasicScheme = "Basic"
  val BearerScheme = "Bearer"

  def apply(scheme: String): WWWAuthenticateChallenge = WWWAuthenticateChallenge(scheme, ListMap.empty)
  def basic: WWWAuthenticateChallenge = WWWAuthenticateChallenge(BasicScheme)
  def basic(realm: String): WWWAuthenticateChallenge = WWWAuthenticateChallenge(BasicScheme).realm(realm)
  def bearer: WWWAuthenticateChallenge = WWWAuthenticateChallenge(BasicScheme)
  def bearer(realm: String): WWWAuthenticateChallenge = WWWAuthenticateChallenge(BasicScheme).realm(realm)
}
