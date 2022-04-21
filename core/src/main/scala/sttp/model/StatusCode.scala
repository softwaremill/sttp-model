package sttp.model

import internal.Validate._

class StatusCode(val code: Int) extends AnyVal {
  def isInformational: Boolean = code / 100 == 1
  def isSuccess: Boolean = code / 100 == 2
  def isRedirect: Boolean = code / 100 == 3
  def isClientError: Boolean = code / 100 == 4
  def isServerError: Boolean = code / 100 == 5

  override def toString: String = code.toString
}

/** For a description of the behavior of `apply`, `safeApply` and `unsafeApply` methods, see [[sttp.model]].
  */
object StatusCode extends StatusCodes {

  /** @throws IllegalArgumentException
    *   If the status code is out of range.
    */
  def unsafeApply(code: Int): StatusCode = safeApply(code).getOrThrow
  def safeApply(code: Int): Either[String, StatusCode] = {
    if (code < 100 || code > 599) Left(s"Status code outside of the allowed range 100-599: $code")
    else Right(apply(code))
  }
  def apply(code: Int): StatusCode = new StatusCode(code)
}

// https://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml
trait StatusCodes {
  val Continue: StatusCode = StatusCode(100)
  val SwitchingProtocols: StatusCode = StatusCode(101)
  val Processing: StatusCode = StatusCode(102)
  val EarlyHints: StatusCode = StatusCode(103)

  val Ok: StatusCode = StatusCode(200)
  val Created: StatusCode = StatusCode(201)
  val Accepted: StatusCode = StatusCode(202)
  val NonAuthoritativeInformation: StatusCode = StatusCode(203)
  val NoContent: StatusCode = StatusCode(204)
  val ResetContent: StatusCode = StatusCode(205)
  val PartialContent: StatusCode = StatusCode(206)
  val MultiStatus: StatusCode = StatusCode(207)
  val AlreadyReported: StatusCode = StatusCode(208)
  val ImUsed: StatusCode = StatusCode(226)

  val MultipleChoices: StatusCode = StatusCode(300)
  val MovedPermanently: StatusCode = StatusCode(301)
  val Found: StatusCode = StatusCode(302)
  val SeeOther: StatusCode = StatusCode(303)
  val NotModified: StatusCode = StatusCode(304)
  val UseProxy: StatusCode = StatusCode(305)
  val TemporaryRedirect: StatusCode = StatusCode(307)
  val PermanentRedirect: StatusCode = StatusCode(308)

  val BadRequest: StatusCode = StatusCode(400)
  val Unauthorized: StatusCode = StatusCode(401)
  val PaymentRequired: StatusCode = StatusCode(402)
  val Forbidden: StatusCode = StatusCode(403)
  val NotFound: StatusCode = StatusCode(404)
  val MethodNotAllowed: StatusCode = StatusCode(405)
  val NotAcceptable: StatusCode = StatusCode(406)
  val ProxyAuthenticationRequired: StatusCode = StatusCode(407)
  val RequestTimeout: StatusCode = StatusCode(408)
  val Conflict: StatusCode = StatusCode(409)
  val Gone: StatusCode = StatusCode(410)
  val LengthRequired: StatusCode = StatusCode(411)
  val PreconditionFailed: StatusCode = StatusCode(412)
  val PayloadTooLarge: StatusCode = StatusCode(413)
  val UriTooLong: StatusCode = StatusCode(414)
  val UnsupportedMediaType: StatusCode = StatusCode(415)
  val RangeNotSatisfiable: StatusCode = StatusCode(416)
  val ExpectationFailed: StatusCode = StatusCode(417)
  val MisdirectedRequest: StatusCode = StatusCode(421)
  val UnprocessableEntity: StatusCode = StatusCode(422)
  val Locked: StatusCode = StatusCode(423)
  val FailedDependency: StatusCode = StatusCode(424)
  val UpgradeRequired: StatusCode = StatusCode(426)
  val PreconditionRequired: StatusCode = StatusCode(428)
  val TooManyRequests: StatusCode = StatusCode(429)
  val RequestHeaderFieldsTooLarge: StatusCode = StatusCode(431)
  val UnavailableForLegalReasons: StatusCode = StatusCode(451)

  val InternalServerError: StatusCode = StatusCode(500)
  val NotImplemented: StatusCode = StatusCode(501)
  val BadGateway: StatusCode = StatusCode(502)
  val ServiceUnavailable: StatusCode = StatusCode(503)
  val GatewayTimeout: StatusCode = StatusCode(504)
  val HttpVersionNotSupported: StatusCode = StatusCode(505)
  val VariantAlsoNegotiates: StatusCode = StatusCode(506)
  val InsufficientStorage: StatusCode = StatusCode(507)
  val LoopDetected: StatusCode = StatusCode(508)
  val NotExtended: StatusCode = StatusCode(510)
  val NetworkAuthenticationRequired: StatusCode = StatusCode(511)
}
