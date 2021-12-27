package sttp.model

import scala.collection.immutable.ListMap

object AuthenticationSchemes {

  val BasicScheme: String = Basic.name
  val BearerScheme: String = Bearer.name
  val DigestScheme: String = Digest.name
  val supportedSchemes: List[String] = List(Basic.name, Bearer.name, Digest.name)

  object Basic {

    val maxParametersCount = 2
    val name = "Basic"

    private val realm: String = "realm"
    private val charset: String = "charset"

    def getParams(params: Map[String, String]): ListMap[String, String] =
      ListMap(
        realm -> params.getOrElse(realm, ""),
        charset -> params.getOrElse(charset, "")
      )
        .filter(_._2.nonEmpty)
  }

  object Bearer {
    val maxParametersCount = 5
    val name = "Bearer"

    private val realm: String = "realm"
    private val scope: String = "scope"
    private val error: String = "error"
    private val errorDescription: String = "error_description"
    private val errorUri: String = "error_uri"

    def getParams(params: Map[String, String]): ListMap[String, String] =
      ListMap(
        realm -> params.getOrElse(realm, ""),
        scope -> params.getOrElse(scope, ""),
        error -> params.getOrElse(error, ""),
        errorDescription -> params.getOrElse(errorDescription, ""),
        errorUri -> params.getOrElse(errorUri, "")
      )
        .filter(_._2.nonEmpty)
  }

  object Digest {
    val maxParametersCount = 9
    val name = "Digest"

    private val realm: String = "realm"
    private val domain: String = "domain"
    private val nonce: String = "nonce"
    private val opaque: String = "opaque"
    private val stale: String = "stale"
    private val algorithm: String = "algorithm"
    private val qop: String = "qop"
    private val charset: String = "charset"
    private val userhash: String = "userhash"

    def getParams(params: Map[String, String]): ListMap[String, String] =
      ListMap(
        realm -> params.getOrElse(realm, ""),
        domain -> params.getOrElse(domain, ""),
        nonce -> params.getOrElse(nonce, ""),
        opaque -> params.getOrElse(opaque, ""),
        stale -> params.getOrElse(stale, ""),
        algorithm -> params.getOrElse(algorithm, ""),
        qop -> params.getOrElse(qop, ""),
        charset -> params.getOrElse(charset, ""),
        userhash -> params.getOrElse(userhash, "")
      )
        .filter(_._2.nonEmpty)
  }
}
