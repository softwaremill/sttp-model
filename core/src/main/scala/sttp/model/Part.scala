package sttp.model

import scala.collection.immutable.Seq
import Part._

/** A decoded representation of a multipart part.
  */
case class Part[+T](
    name: String,
    body: T,
    otherDispositionParams: Map[String, String],
    headers: Seq[Header]
) extends HasHeaders {
  def dispositionParam(k: String, v: String): Part[T] = copy(otherDispositionParams = otherDispositionParams + (k -> v))

  def fileName(v: String): Part[T] = dispositionParam(FileNameDispositionParam, v)
  def fileName: Option[String] = otherDispositionParams.get(FileNameDispositionParam)

  def contentType(v: MediaType): Part[T] = header(Header.contentType(v), replaceExisting = true)
  def contentType(v: String): Part[T] = header(Header(HeaderNames.ContentType, v), replaceExisting = true)
  // enumerate all variants so that overload resolution works correctly
  override def contentType: Option[String] = super.contentType

  /** Adds the given header to the end of the headers sequence.
    * @param replaceExisting
    *   If there's already a header with the same name, should it be dropped?
    */
  def header(h: Header, replaceExisting: Boolean = false): Part[T] = {
    val current = if (replaceExisting) headers.filterNot(_.is(h.name)) else headers
    this.copy(headers = current :+ h)
  }
  def header(k: String, v: String): Part[T] = header(Header(k, v))

  /** Adds the given header to the end of the headers sequence.
    * @param replaceExisting
    *   If there's already a header with the same name, should it be dropped?
    */
  def header(k: String, v: String, replaceExisting: Boolean): Part[T] =
    header(Header(k, v), replaceExisting)

  // enumerate all variants so that overload resolution works correctly
  override def header(h: String): Option[String] = super.header(h)

  def contentDispositionHeaderValue: String =
    "form-data; " + dispositionParamsSeq.map { case (k, v) => s"""$k="$v"""" }.mkString("; ")

  def dispositionParams: Map[String, String] = dispositionParamsSeq.toMap

  // some servers require 'name' disposition parameter to be first
  // see: https://stackoverflow.com/questions/20261088/certain-order-of-fields-in-content-disposition-with-jersey-client
  def dispositionParamsSeq: Seq[(String, String)] = (NameDispositionParam -> name) :: otherDispositionParams.toList
}

object Part {
  def apply[T](
      name: String,
      body: T,
      contentType: Option[MediaType] = None,
      fileName: Option[String] = None,
      otherDispositionParams: Map[String, String] = Map.empty,
      otherHeaders: Seq[Header] = Nil
  ): Part[T] = {
    val p1 = Part(name, body, otherDispositionParams, otherHeaders)
    val p2 = contentType.map(p1.contentType).getOrElse(p1)
    fileName.map(p2.fileName).getOrElse(p2)
  }

  val NameDispositionParam = "name"
  val FileNameDispositionParam = "filename"
}
