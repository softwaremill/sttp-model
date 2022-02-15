package sttp.model.headers

import AuthenticationScheme.{Basic, Bearer, Digest}
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

  val RealmParam: String = "realm"
  val CharsetParam: String = "charset"

  @deprecated(message = "Use AuthenticationScheme.Basic.name", since = "1.4.23")
  val BasicScheme: String = Basic.name
  @deprecated(message = "Use AuthenticationScheme.Bearer.name", since = "1.4.23")
  val BearerScheme: String = Bearer.name
  @deprecated(message = "Use AuthenticationScheme.Digest.name", since = "1.4.23")
  val DigestScheme: String = Digest.name

  def parseSingle(str: String): Either[String, WWWAuthenticateChallenge] = {
    str.trim.replaceFirst(" ", "_").split("_") match {
      case Array(x, possibleParams) =>
        if (AuthenticationScheme.supportedNames.exists(possibleParams.contains))
          Left(s"Multiple challenges in single header not supported but found in: $str")
        else {
          val params = creteParamsMap(possibleParams.trim)
          x.trim match {
            case Basic.name =>
              if (params.size > Basic.maxParametersCount)
                Left(s"Too many params for Basic in: $possibleParams")
              else
                Right(
                  WWWAuthenticateChallenge(
                    Basic.name,
                    Basic.getParams(params)
                  )
                )
            case Bearer.name =>
              if (params.size > Bearer.maxParametersCount)
                Left(s"Too many params for Bearer in: $possibleParams")
              else
                Right(
                  WWWAuthenticateChallenge(
                    Bearer.name,
                    Bearer.getParams(params)
                  )
                )
            case Digest.name =>
              Digest
                .paramsValid(params)
                .right
                .map(_ =>
                  WWWAuthenticateChallenge(
                    Digest.name,
                    Digest.getParams(params)
                  )
                )
            case _ => Left(s"$x authentication scheme not supported")
          }
        }
      case Array(schema) =>
        schema.trim match {
          case Basic.name  => Right(WWWAuthenticateChallenge(schema))
          case Bearer.name => Right(WWWAuthenticateChallenge(schema))
          case Digest.name => Right(WWWAuthenticateChallenge(schema))
          case _           => Left(s"$schema authentication scheme not supported")
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
  def basic: WWWAuthenticateChallenge = WWWAuthenticateChallenge(Basic.name)
  def basic(realm: String): WWWAuthenticateChallenge = WWWAuthenticateChallenge(Basic.name).realm(realm)
  def bearer: WWWAuthenticateChallenge = WWWAuthenticateChallenge(Bearer.name)
  def bearer(realm: String): WWWAuthenticateChallenge = WWWAuthenticateChallenge(Bearer.name).realm(realm)
}
