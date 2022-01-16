package sttp.model.headers

import sttp.model.AuthenticationSchemes
import sttp.model.AuthenticationSchemes.{Basic, Bearer, Digest}
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
  val BasicScheme: String = Basic.name
  val BearerScheme: String = Bearer.name
  val DigestScheme: String = Digest.name

  def parseSingle(str: String): Either[String, WWWAuthenticateChallenge] = {
    str.trim.replaceFirst(" ", "_").split("_") match {
      case Array(x, possibleParams) =>
        if (AuthenticationSchemes.supportedSchemes.exists(possibleParams.contains))
          Left(s"Multiple challenges in single header not supported but found in: $str")
        else {
          val params = creteParamsMap(possibleParams.trim)
          x.trim match {
            case BasicScheme =>
              if (params.size > AuthenticationSchemes.Basic.maxParametersCount)
                Left(s"To much params for Basic in: $possibleParams")
              else
                Right(
                  WWWAuthenticateChallenge(
                    AuthenticationSchemes.Basic.name,
                    AuthenticationSchemes.Basic.getParams(params)
                  )
                )
            case BearerScheme =>
              if (params.size > AuthenticationSchemes.Bearer.maxParametersCount)
                Left(s"To much params for Bearer in: $possibleParams")
              else
                Right(
                  WWWAuthenticateChallenge(
                    AuthenticationSchemes.Bearer.name,
                    AuthenticationSchemes.Bearer.getParams(params)
                  )
                )
            case DigestScheme =>
              if (params.size > AuthenticationSchemes.Digest.maxParametersCount)
                Left(s"To much params for Digest in: $possibleParams")
              else
                Right(
                  WWWAuthenticateChallenge(
                    AuthenticationSchemes.Digest.name,
                    AuthenticationSchemes.Digest.getParams(params)
                  )
                )
            case _ => Left(s"$x authentication scheme not supported")
          }
        }
      case Array(schema) =>
        schema.trim match {
          case BasicScheme  => Right(WWWAuthenticateChallenge(schema))
          case BearerScheme => Right(WWWAuthenticateChallenge(schema))
          case DigestScheme => Right(WWWAuthenticateChallenge(schema))
          case _            => Left(s"$schema authentication scheme not supported")
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
  def basic: WWWAuthenticateChallenge = WWWAuthenticateChallenge(BasicScheme)
  def basic(realm: String): WWWAuthenticateChallenge =
    WWWAuthenticateChallenge(BasicScheme).realm(realm)
  def bearer: WWWAuthenticateChallenge = WWWAuthenticateChallenge(BearerScheme)
  def bearer(realm: String): WWWAuthenticateChallenge =
    WWWAuthenticateChallenge(BearerScheme).realm(realm)
}
