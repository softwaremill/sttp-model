package sttp.model.headers

import sttp.model.Header
import sttp.model.headers.Cookie.SameSite
import sttp.model.internal.Rfc2616.validateToken
import sttp.model.internal.Validate._
import sttp.model.internal.{Rfc2616, Validate}

import java.time.Instant
import scala.util.{Failure, Success, Try}

/** A cookie name-value pair.
  *
  * The `name` and `value` should be already encoded (if necessary), as when serialised, they end up unmodified in the
  * header.
  */
case class Cookie(name: String, value: String) {

  /** @return
    *   Representation of the cookie as in a header value, in the format: `[name]=[value]`.
    */
  override def toString: String = s"$name=$value"
}

/** For a description of the behavior of `apply`, `parse`, `safeApply` and `unsafeApply` methods, see [[sttp.model]].
  */
object Cookie {
  // see: https://stackoverflow.com/questions/1969232/allowed-characters-in-cookies/1969339
  private val AllowedValueCharacters = s"[^${Rfc2616.CTL}]*".r

  private[model] def validateName(name: String): Option[String] = validateToken("Cookie name", name)

  private[model] def validateValue(value: String): Option[String] =
    if (AllowedValueCharacters.unapplySeq(value).isEmpty) {
      Some("Cookie value can not contain control characters")
    } else None

  /** @throws IllegalArgumentException
    *   If the cookie name or value contain illegal characters.
    */
  def unsafeApply(name: String, value: String): Cookie = safeApply(name, value).getOrThrow

  def safeApply(name: String, value: String): Either[String, Cookie] = {
    Validate.all(validateName(name), validateValue(value))(new Cookie(name, value))
  }

  /** Parse the cookie, represented as a header value (in the format: `[name]=[value]`).
    */
  def parse(s: String): Either[String, List[Cookie]] = {
    val ss = s.split(";")
    val cs = List.newBuilder[Cookie]
    var i = 0
    while (i < ss.length) {
      val vs = ss(i).split("=", 2)
      (if (vs.length == 1) Cookie.safeApply(vs(0).trim, "")
       else Cookie.safeApply(vs(0).trim, vs(1).trim)) match {
        case Right(c)    => cs += c
        case Left(error) => return Left(error)
      }
      i += 1
    }
    Right(cs.result())
  }

  def unsafeParse(s: String): List[Cookie] = parse(s).getOrThrow

  /** @return
    *   Representation of the cookies as in a header value, in the format: `[name]=[value]; [name]=[value]; ...`.
    */
  def toString(cs: List[Cookie]): String = cs.map(_.toString).mkString("; ")

  sealed trait SameSite
  object SameSite {
    case object Lax extends SameSite { override def toString = "Lax" }
    case object Strict extends SameSite { override def toString = "Strict" }
    case object None extends SameSite { override def toString = "None" }
  }
}

case class CookieValueWithMeta(
    value: String,
    expires: Option[Instant],
    maxAge: Option[Long],
    domain: Option[String],
    path: Option[String],
    secure: Boolean,
    httpOnly: Boolean,
    sameSite: Option[SameSite],
    otherDirectives: Map[String, Option[String]]
)

object CookieValueWithMeta {
  private val AllowedDirectiveValueCharacters = s"""[^;${Rfc2616.CTL}]*""".r

  private[model] def validateDirectiveValue(directiveName: String, value: String): Option[String] =
    if (AllowedDirectiveValueCharacters.unapplySeq(value).isEmpty) {
      Some(s"Value of directive $directiveName name can contain any characters except ; and control characters")
    } else None

  def unsafeApply(
      value: String,
      expires: Option[Instant] = None,
      maxAge: Option[Long] = None,
      domain: Option[String] = None,
      path: Option[String] = None,
      secure: Boolean = false,
      httpOnly: Boolean = false,
      sameSite: Option[SameSite] = None,
      otherDirectives: Map[String, Option[String]] = Map.empty
  ): CookieValueWithMeta =
    safeApply(value, expires, maxAge, domain, path, secure, httpOnly, sameSite, otherDirectives).getOrThrow

  def safeApply(
      value: String,
      expires: Option[Instant] = None,
      maxAge: Option[Long] = None,
      domain: Option[String] = None,
      path: Option[String] = None,
      secure: Boolean = false,
      httpOnly: Boolean = false,
      sameSite: Option[SameSite] = None,
      otherDirectives: Map[String, Option[String]] = Map.empty
  ): Either[String, CookieValueWithMeta] =
    Cookie
      .validateValue(value)
      .orElse(path.flatMap(validateDirectiveValue("path", _)))
      .orElse(domain.flatMap(validateDirectiveValue("domain", _))) match {
      case None => Right(apply(value, expires, maxAge, domain, path, secure, httpOnly, sameSite, otherDirectives))
      case Some(error) => Left(error)
    }
}

/** A cookie name-value pair with directives.
  *
  * All `String` values should be already encoded (if necessary), as when serialised, they end up unmodified in the
  * header.
  */
case class CookieWithMeta(
    name: String,
    valueWithMeta: CookieValueWithMeta
) {
  def value: String = valueWithMeta.value
  def expires: Option[Instant] = valueWithMeta.expires
  def maxAge: Option[Long] = valueWithMeta.maxAge
  def domain: Option[String] = valueWithMeta.domain
  def path: Option[String] = valueWithMeta.path
  def secure: Boolean = valueWithMeta.secure
  def httpOnly: Boolean = valueWithMeta.httpOnly
  def sameSite: Option[SameSite] = valueWithMeta.sameSite
  def otherDirectives: Map[String, Option[String]] = valueWithMeta.otherDirectives

  def value(v: String): CookieWithMeta = copy(valueWithMeta = valueWithMeta.copy(value = v))
  def expires(v: Option[Instant]): CookieWithMeta = copy(valueWithMeta = valueWithMeta.copy(expires = v))
  def maxAge(v: Option[Long]): CookieWithMeta = copy(valueWithMeta = valueWithMeta.copy(maxAge = v))
  def domain(v: Option[String]): CookieWithMeta = copy(valueWithMeta = valueWithMeta.copy(domain = v))
  def path(v: Option[String]): CookieWithMeta = copy(valueWithMeta = valueWithMeta.copy(path = v))
  def secure(v: Boolean): CookieWithMeta = copy(valueWithMeta = valueWithMeta.copy(secure = v))
  def httpOnly(v: Boolean): CookieWithMeta = copy(valueWithMeta = valueWithMeta.copy(httpOnly = v))
  def sameSite(s: Option[SameSite]): CookieWithMeta = copy(valueWithMeta = valueWithMeta.copy(sameSite = s))
  def otherDirective(v: (String, Option[String])): CookieWithMeta =
    copy(valueWithMeta = valueWithMeta.copy(otherDirectives = otherDirectives + v))

  /** @return
    *   Representation of the cookie as in a header value, in the format: `[name]=[value]; [directive]=[value]; ...`.
    */
  override def toString: String = {
    val sb = new java.lang.StringBuilder(64)
    sb.append(name).append('=').append(value)
    expires match {
      case x: Some[Instant] => sb.append("; Expires=").append(Header.toHttpDateString(x.value))
      case _                => ()
    }
    maxAge match {
      case x: Some[Long] => sb.append("; Max-Age=").append(x.value)
      case _             => ()
    }
    domain match {
      case x: Some[String] => sb.append("; Domain=").append(x.value)
      case _               => ()
    }
    path match {
      case x: Some[String] => sb.append("; Path=").append(x.value)
      case _               => ()
    }
    if (secure) sb.append("; Secure")
    else ()
    if (httpOnly) sb.append("; HttpOnly")
    else ()
    sameSite match {
      case x: Some[SameSite] => sb.append("; SameSite=").append(x.value)
      case _                 => ()
    }
    otherDirectives.foreach { case (k, optV) =>
      sb.append("; ").append(k)
      optV match {
        case x: Some[String] => sb.append('=').append(x.value)
        case _               => ()
      }
    }
    sb.toString
  }
}

object CookieWithMeta {
  def unsafeApply(
      name: String,
      value: String,
      expires: Option[Instant] = None,
      maxAge: Option[Long] = None,
      domain: Option[String] = None,
      path: Option[String] = None,
      secure: Boolean = false,
      httpOnly: Boolean = false,
      sameSite: Option[SameSite] = None,
      otherDirectives: Map[String, Option[String]] = Map.empty
  ): CookieWithMeta =
    safeApply(name, value, expires, maxAge, domain, path, secure, httpOnly, sameSite, otherDirectives).getOrThrow

  def safeApply(
      name: String,
      value: String,
      expires: Option[Instant] = None,
      maxAge: Option[Long] = None,
      domain: Option[String] = None,
      path: Option[String] = None,
      secure: Boolean = false,
      httpOnly: Boolean = false,
      sameSite: Option[SameSite] = None,
      otherDirectives: Map[String, Option[String]] = Map.empty
  ): Either[String, CookieWithMeta] =
    Cookie.validateName(name) match {
      case None =>
        CookieValueWithMeta
          .safeApply(value, expires, maxAge, domain, path, secure, httpOnly, sameSite, otherDirectives)
          .map(v => apply(name, v))
      case Some(e) => Left(e)
    }

  def apply(
      name: String,
      value: String,
      expires: Option[Instant] = None,
      maxAge: Option[Long] = None,
      domain: Option[String] = None,
      path: Option[String] = None,
      secure: Boolean = false,
      httpOnly: Boolean = false,
      sameSite: Option[SameSite] = None,
      otherDirectives: Map[String, Option[String]] = Map.empty
  ): CookieWithMeta =
    apply(
      name,
      CookieValueWithMeta(value, expires, maxAge, domain, path, secure, httpOnly, sameSite, otherDirectives)
    )

  // https://tools.ietf.org/html/rfc6265#section-4.1.1
  /** Parse the cookie, represented as a header value (in the format: `[name]=[value]; [directive]=[value]; ...`).
    */
  def parse(s: String): Either[String, CookieWithMeta] = {
    def splitkv(kv: String): (String, Option[String]) =
      (kv.split("=", 2): @unchecked) match {
        case Array(v1)     => (v1.trim, None)
        case Array(v1, v2) => (v1.trim, Some(v2.trim))
      }

    val components = s.split(";")
    val (first, other) = (components.head.trim, components.tail)
    val (name, value) = splitkv(first)
    var result: Either[String, CookieWithMeta] = Right(CookieWithMeta.apply(name, value.getOrElse("")))
    other.map(splitkv).foreach {
      case (ci"expires", Some(v)) =>
        Header.parseHttpDate(v) match {
          case Right(expires) => result = result.map(_.expires(Some(expires)))
          case Left(_) => result = Left(s"Expires cookie directive is not a valid RFC1123 or RFC850 datetime: $v")
        }
      case (ci"max-age", Some(v)) =>
        Try(v.toLong) match {
          case Success(maxAge) => result = result.map(_.maxAge(Some(maxAge)))
          case Failure(_)      => result = Left(s"Max-Age cookie directive is not a number: $v")
        }
      case (ci"domain", v)   => result = result.map(_.domain(Some(v.getOrElse(""))))
      case (ci"path", v)     => result = result.map(_.path(Some(v.getOrElse(""))))
      case (ci"secure", _)   => result = result.map(_.secure(true))
      case (ci"httponly", _) => result = result.map(_.httpOnly(true))
      case (ci"samesite", Some(v)) =>
        v.trim match {
          case ci"lax"    => result = result.map(_.sameSite(Some(SameSite.Lax)))
          case ci"strict" => result = result.map(_.sameSite(Some(SameSite.Strict)))
          case ci"none"   => result = result.map(_.sameSite(Some(SameSite.None)))
          case _          => result = Left(s"Same-Site cookie directive is not an allowed value: $v")
        }
      case (k, v) => result = result.map(_.otherDirective((k, v)))
    }
    result
  }

  def unsafeParse(s: String): CookieWithMeta = parse(s).getOrThrow

  private implicit class StringInterpolations(sc: StringContext) {
    class CaseInsensitiveStringMatcher {
      def unapply(other: String): Boolean = sc.parts.mkString.equalsIgnoreCase(other)
    }
    def ci: CaseInsensitiveStringMatcher = new CaseInsensitiveStringMatcher
  }
}
