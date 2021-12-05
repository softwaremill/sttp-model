package sttp.model.headers

import sttp.model.{AuthenticationSchemes, Basic, Bearer, Digest}

import scala.collection.immutable.ListMap

case class WWWAuthenticateChallenge(scheme: String, params: ListMap[String, String]) {
  override def toString: String = {
    val paramsAsString = params.map { case (k, v) => s"$k=$v" }.mkString(", ")
    val sep = if (paramsAsString.nonEmpty) " " else ""
    val d = s"$scheme$sep$paramsAsString"
    d
  }

  def realm: Option[String] = param(AuthenticationSchemes.RealmParam)
  def realm(r: String): WWWAuthenticateChallenge = addParam(AuthenticationSchemes.RealmParam, r)

  def charset: Option[String] = param(AuthenticationSchemes.CharsetParam)
  def charset(c: String): WWWAuthenticateChallenge = addParam(AuthenticationSchemes.CharsetParam, c)

  def param(key: String): Option[String] = params.get(key)
  def addParam(key: String, value: String): WWWAuthenticateChallenge = copy(params = params + (key -> value))
}

object WWWAuthenticateChallenge {

  def parseSingle(str: String): Either[String, WWWAuthenticateChallenge] = {
    str.trim.replaceFirst(" ", "_").split("_") match {
      case Array(x, possibleParams) =>
        if (AuthenticationSchemes.supportedSchems.forall(possibleParams.contains))
          Left(s"Multiple challenges in single header not supported but found in: $str")
        else {
          val params = creteParamsMap(possibleParams.trim)
          x.trim match {
            case AuthenticationSchemes.BasicScheme =>
              if (params.size > Basic.maxParametersCount) Left(s"To much params for Basic in: $possibleParams")
              else Right(WWWAuthenticateChallenge(Basic.name, Basic.getParams(params)))
            case AuthenticationSchemes.BearerScheme =>
              if (params.size > Bearer.maxParametersCount) Left(s"To much params for Bearer in: $possibleParams")
              else Right(WWWAuthenticateChallenge(Bearer.name, Bearer.getParams(params)))
            case AuthenticationSchemes.DigestScheme =>
              if (params.size > Digest.maxParametersCount) Left(s"To much params for Digiset in: $possibleParams")
              else Right(WWWAuthenticateChallenge(Digest.name, Digest.getParams(params)))
            case _ => Left(s"$x authentication scheme not supported")
          }
        }
      case Array(schema) =>
        schema.trim match {
          case AuthenticationSchemes.BasicScheme   => Right(WWWAuthenticateChallenge(schema))
          case AuthenticationSchemes.BearerScheme  => Right(WWWAuthenticateChallenge(schema))
          case AuthenticationSchemes.DigestScheme => Right(WWWAuthenticateChallenge(schema))
          case _                                   => Left(s"$schema authentication scheme not supported")
        }
      case _ => Left(s"$str is not valid value of header")
    }
  }

  private def creteParamsMap(possibleParams: String): Map[String, String] =
    possibleParams
      .split("\",")
      .map(s => {
        val strings = s.split("=")
        (strings(0).trim.replace("\"", ""), strings(1).trim.replace("\"", ""))
      })
      .toMap

  def apply(scheme: String): WWWAuthenticateChallenge = WWWAuthenticateChallenge(scheme, ListMap.empty)
  def basic: WWWAuthenticateChallenge = WWWAuthenticateChallenge(AuthenticationSchemes.BearerScheme)
  def basic(realm: String): WWWAuthenticateChallenge =
    WWWAuthenticateChallenge(AuthenticationSchemes.BasicScheme).realm(realm)
  def bearer: WWWAuthenticateChallenge = WWWAuthenticateChallenge(AuthenticationSchemes.BearerScheme)
  def bearer(realm: String): WWWAuthenticateChallenge =
    WWWAuthenticateChallenge(AuthenticationSchemes.BearerScheme).realm(realm)
}
