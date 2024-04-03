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

  /** Returns the value of the `Content-Disposition` header, which should be used when sending this part in a multipart
    * request.
    *
    * The syntax is specified by [[https://datatracker.ietf.org/doc/html/rfc6266#section-4.1 RFC6266 section 4.1]]. For
    * safety and simplicity, disposition parameter values are represented as `quoted-string`, defined in
    * [[https://datatracker.ietf.org/doc/html/rfc9110#section-5.6.4 RFC9110 section 5.6.4]].
    *
    * `quoted-string` allows usage of visible ASCII characters (`%x21-7E`), except for `"` and `\`, which must be
    * escaped with a backslash. Additionally, space and horizontal tab is allowed, as well as octets `0x80-FF`
    * (`obs-data`). In practice this means that - while not explicitly allowed - non-ASCII UTF-8 characters are valid
    * according to this grammar. Additionally, [[https://datatracker.ietf.org/doc/html/rfc6532#section-3.2 RFC6532]]
    * makes it more explicit that non-ASCII UTF-8 is allowed. Control characters are not allowed.
    *
    * This method makes sure that `"` and `\` are escaped, while leaving possible rejection of forbidden characters to
    * lower layers (`sttp` backends).
    */
  def contentDispositionHeaderValue: String = {
    def escape(str: String): String = str.flatMap {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case c    => c.toString
    }
    "form-data; " + dispositionParamsSeq.map { case (k, v) => s"""$k="${escape(v)}"""" }.mkString("; ")
  }

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
