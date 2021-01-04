package sttp.model

import sttp.model.HeaderNames.SensitiveHeaders
import sttp.model.internal.Validate
import sttp.model.internal.Rfc2616.validateToken
import sttp.model.internal.Validate._

import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import scala.concurrent.duration.FiniteDuration
import scala.util.hashing.MurmurHash3

/** An HTTP header. The [[name]] property is case-insensitive during equality checks.
  *
  * To compare if two headers have the same name, use the [[is]] method, which does a case-insensitive check,
  * instead of comparing the [[name]] property.
  *
  * The [[name]] and [[value]] should be already encoded (if necessary), as when serialised, they end up unmodified in
  * the header.
  */
class Header(val name: String, val value: String) {

  /** Check if the name of this header is the same as the given one. The names are compared in a case-insensitive way.
    */
  def is(otherName: String): Boolean = name.equalsIgnoreCase(otherName)

  /** @return Representation in the format: `[name]: [value]`.
    */
  override def toString: String = s"$name: $value"
  override def hashCode(): Int = MurmurHash3.mixLast(name.toLowerCase.hashCode, value.hashCode)
  override def equals(that: Any): Boolean =
    that match {
      case h: AnyRef if this.eq(h) => true
      case h: Header               => is(h.name) && (value == h.value)
      case _                       => false
    }

  /** @return Representation in the format: `[name]: [value]`. If the header is sensitive
    *         (see [[HeaderNames.SensitiveHeaders]]), the value is omitted.
    */
  def toStringSafe(sensitiveHeaders: Set[String] = SensitiveHeaders): String =
    s"$name: ${if (HeaderNames.isSensitive(name, sensitiveHeaders)) "***" else value}"
}

/** For a description of the behavior of `apply`, `safeApply` and `unsafeApply` methods, see [[sttp.model]].
  */
object Header {
  def unapply(h: Header): Option[(String, String)] = Some((h.name, h.value))

  /** @throws IllegalArgumentException If the header name contains illegal characters.
    */
  def unsafeApply(name: String, value: String): Header = safeApply(name, value).getOrThrow

  def safeApply(name: String, value: String): Either[String, Header] = {
    Validate.all(validateToken("Header name", name))(apply(name, value))
  }

  def apply(name: String, value: String): Header = new Header(name, value)

  //

  def accept(mediaType: MediaType, additionalMediaTypes: MediaType*): Header = accept(
    s"${(mediaType :: additionalMediaTypes.toList).map(_.noCharset).mkString(", ")}"
  )
  def accept(mediaRanges: String): Header = Header(HeaderNames.Accept, mediaRanges)
  def acceptCharset(charsetRanges: String): Header = Header(HeaderNames.AcceptCharset, charsetRanges)
  def acceptEncoding(encodingRanges: String): Header = Header(HeaderNames.AcceptEncoding, encodingRanges)
  def accessControlAllowCredentials(allow: Boolean): Header =
    Header(HeaderNames.AccessControlAllowCredentials, allow.toString)
  def accessControlAllowHeaders(headerNames: String*): Header =
    Header(HeaderNames.AccessControlAllowHeaders, headerNames.mkString(", "))
  def accessControlAllowMethods(methods: Method*): Header =
    Header(HeaderNames.AccessControlAllowMethods, methods.map(_.method).mkString(", "))
  def accessControlAllowOrigin(originRange: String): Header =
    Header(HeaderNames.AccessControlAllowOrigin, originRange)
  def accessControlExposeHeaders(headerNames: String*): Header =
    Header(HeaderNames.AccessControlExposeHeaders, headerNames.mkString(", "))
  def accessControlMaxAge(deltaSeconds: Long): Header =
    Header(HeaderNames.AccessControlMaxAge, deltaSeconds.toString)
  def accessControlRequestHeaders(headerNames: String*): Header =
    Header(HeaderNames.AccessControlRequestHeaders, headerNames.mkString(", "))
  def accessControlRequestMethod(method: Method): Header =
    Header(HeaderNames.AccessControlRequestMethod, method.toString)
  def authorization(authType: String, credentials: String): Header =
    Header(HeaderNames.Authorization, s"$authType $credentials")
  def cacheControl(
      maxAge: Option[FiniteDuration] = None,
      maxStale: Option[Option[FiniteDuration]] = None,
      minFresh: Option[FiniteDuration] = None,
      noCache: Boolean = false,
      noStore: Boolean = false,
      noTransform: Boolean = false,
      onlyIfCached: Boolean = false,
      mustRevalidate: Boolean = false,
      public: Boolean = false,
      `private`: Boolean = false,
      proxyRevalidate: Boolean = false,
      sMaxage: Option[FiniteDuration] = None,
      immutable: Boolean = false,
      staleWhileRevalidate: Option[FiniteDuration] = None,
      staleIfError: Option[FiniteDuration] = None
  ): Header = {
    val values = scala.collection.mutable.SortedSet[String]()
    maxAge.foreach(v => values.add(s"max-age=${v.toSeconds}"))
    maxStale.foreach {
      case Some(v) => values.add(s"max-stale=${v.toSeconds}")
      case None    => values.add("max-stale")
    }
    minFresh.foreach(v => values.add(s"min-fresh=${v.toSeconds}"))
    if (noCache) values.add(s"no-cache")
    if (noStore) values.add(s"no-store")
    if (noTransform) values.add(s"no-transform")
    if (onlyIfCached) values.add(s"only-if-cached")
    if (mustRevalidate) values.add(s"must-revalidate")
    if (public) values.add(s"public")
    if (`private`) values.add(s"private")
    if (proxyRevalidate) values.add(s"proxy-revalidate")
    sMaxage.foreach(v => values.add(s"s-maxage=${v.toSeconds}"))
    if (immutable) values.add(s"immutable")
    staleWhileRevalidate.foreach(v => values.add(s"stale-while-revalidate=${v.toSeconds}"))
    staleIfError.foreach(v => values.add(s"stale-if-error=${v.toSeconds}"))
    Header(HeaderNames.CacheControl, values.mkString(", "))
  }
  def contentLength(length: Long): Header = Header(HeaderNames.ContentLength, length.toString)
  def contentEncoding(encoding: String): Header = Header(HeaderNames.ContentEncoding, encoding)
  def contentType(mediaType: MediaType): Header = Header(HeaderNames.ContentType, mediaType.toString)
  def cookie(firstCookie: Cookie, otherCookies: Cookie*): Header =
    Header(HeaderNames.Cookie, (firstCookie +: otherCookies).map(_.toString).mkString("; "))
  def etag(tag: String, weak: Boolean = false): Header =
    Header(HeaderNames.Etag, s"""${if (weak) "W/" else ""}"$tag"""")
  def expires(i: Instant): Header =
    Header(HeaderNames.Expires, DateTimeFormatter.RFC_1123_DATE_TIME.format(i.atZone(GMT)))
  def lastModified(i: Instant): Header =
    Header(HeaderNames.LastModified, DateTimeFormatter.RFC_1123_DATE_TIME.format(i.atZone(GMT)))
  def location(uri: String): Header = Header(HeaderNames.Location, uri)
  def location(uri: Uri): Header = Header(HeaderNames.Location, uri.toString)
  def proxyAuthorization(authType: String, credentials: String): Header =
    Header(HeaderNames.ProxyAuthorization, s"$authType $credentials")
  def setCookie(cookie: CookieWithMeta): Header = Header(HeaderNames.SetCookie, cookie.toString)
  def userAgent(userAgent: String): Header = Header(HeaderNames.UserAgent, userAgent)
  def xForwardedFor(firstAddress: String, otherAddresses: String*): Header =
    Header(HeaderNames.XForwardedFor, (firstAddress +: otherAddresses).mkString(", "))

  private val GMT = ZoneId.of("GMT")
}
