package sttp.model

import sttp.model.headers.{AcceptCharsetHeader, AcceptHeader, CookieWithMeta}

import scala.collection.immutable.Seq
import scala.util.Try

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
  def contentLength: Option[Long] = header(HeaderNames.ContentLength).flatMap(cl => Try(cl.toLong).toOption)

  def cookies: Seq[Either[String, CookieWithMeta]] = headers(HeaderNames.SetCookie).map(h => CookieWithMeta.parse(h))
  def unsafeCookies: Seq[CookieWithMeta] =
    cookies.map(_.fold(e => throw new RuntimeException(e), identity[CookieWithMeta]))

  def accept: Seq[(MediaType, Float)] = AcceptHeader parse headers
  def acceptCharset: Seq[(String, Float)] = AcceptCharsetHeader parse headers
}
