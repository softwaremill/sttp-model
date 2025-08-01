package sttp.model

import sttp.model.headers.CookieWithMeta
import sttp.model.internal.ParseUtils

import scala.collection.immutable.Seq

case class Headers(headers: Seq[Header]) extends HasHeaders {
  override def toString: String = s"Headers(${Headers.toStringSafe(headers)})"
}
object Headers {
  def toStringSafe(headers: Seq[Header], sensitiveHeaders: Set[String] = HeaderNames.SensitiveHeaders): Seq[String] =
    headers.map(_.toStringSafe(sensitiveHeaders))
}

trait HasHeaders {
  def headers: Seq[Header]
  def header(h: String): Option[String] = headers.find(_.is(h)).map(_.value)
  def headers(h: String): Seq[String] = headers.filter(_.is(h)).map(_.value)

  def contentType: Option[String] = header(HeaderNames.ContentType)

  /** The content length parsed into a long, if present. If the content length is not a number, `None` is returned. */
  def contentLength: Option[Long] = header(HeaderNames.ContentLength).flatMap(ParseUtils.toLongOption)

  def cookies: Seq[Either[String, CookieWithMeta]] = headers(HeaderNames.SetCookie).map(h => CookieWithMeta.parse(h))
  def unsafeCookies: Seq[CookieWithMeta] =
    cookies.map(_.fold(e => throw new RuntimeException(e), identity[CookieWithMeta]))
}
