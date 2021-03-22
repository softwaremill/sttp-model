package sttp.model

import sttp.model.MediaType.Wildcard
import sttp.model.internal.Rfc2616._
import sttp.model.internal.Validate._
import sttp.model.internal.{Patterns, Validate}

import java.nio.charset.Charset

case class MediaType(
    mainType: String,
    subType: String,
    charset: Option[String] = None,
    parameters: Map[String, String] = Map.empty
) {
  def charset(c: Charset): MediaType = charset(c.name())
  def charset(c: String): MediaType = copy(charset = Some(c))
  def noCharset: MediaType = copy(charset = None)

  def matches(other: MediaType): Boolean = {
    val typeMatches = (this, other) match {
      case (MediaType(Wildcard, Wildcard, _, _), _)                                                              => true
      case (_, MediaType(Wildcard, Wildcard, _, _))                                                              => true
      case (MediaType(Wildcard, _, _, _), MediaType(Wildcard, _, _, _))                                          => true
      case (MediaType(mainA, Wildcard, _, _), MediaType(mainB, Wildcard, _, _)) if mainA.equalsIgnoreCase(mainB) => true
      case (MediaType(mainA, Wildcard, _, _), MediaType(mainB, _, _, _)) if mainA.equalsIgnoreCase(mainB)        => true
      case (MediaType(mainA, _, _, _), MediaType(mainB, Wildcard, _, _)) if mainA.equalsIgnoreCase(mainB)        => true
      case (MediaType(mainA, subA, _, _), MediaType(mainB, subB, _, _))
          if mainA.equalsIgnoreCase(mainB) && subA.equalsIgnoreCase(subB) =>
        true
      case _ => false
    }

    if (typeMatches) {
      if (this.isCharsetAny || other.isCharsetAny) true
      else this.charset.map(_.toLowerCase) == other.charset.map(_.toLowerCase)
    } else false
  }

  def matchesExact(other: MediaType): Boolean = {
    this.mainType.equalsIgnoreCase(other.mainType) &&
    this.subType.equalsIgnoreCase(other.subType) &&
    this.charset.map(_.toLowerCase) == other.charset.map(_.toLowerCase)
  }

  def isMainTypeAny: Boolean = mainType == Wildcard
  def isSubTypeAny: Boolean = subType == Wildcard
  def isTypeAny: Boolean = isMainTypeAny && isSubTypeAny
  def isCharsetAny: Boolean = charset.forall(_ == Wildcard)

  override def toString: String = s"$mainType/$subType" + charset.fold("")(c => s"; charset=$c") +
    parameters.foldLeft("") { case (s, (p, v)) => if (p == "charset") s else s"$s; $p=$v" }
}

/** For a description of the behavior of `apply`, `parse`, `safeApply` and `unsafeApply` methods, see [[sttp.model]].
  */
object MediaType extends MediaTypes {

  /** @throws IllegalArgumentException If the main type or subt type contain illegal characters.
    */
  def unsafeApply(
      mainType: String,
      subType: String,
      charset: Option[String] = None,
      parameters: Map[String, String] = Map.empty
  ): MediaType =
    safeApply(mainType, subType, charset, parameters).getOrThrow

  def safeApply(
      mainType: String,
      subType: String,
      charset: Option[String] = None,
      parameters: Map[String, String] = Map.empty
  ): Either[String, MediaType] = {
    Validate.all(
      Seq(
        validateToken("Main type", mainType),
        validateToken("Sub type", subType),
        charset.flatMap(validateToken("Charset", _))
      ) ++ parameters.map { case (p, v) => validateToken(p, v) }: _*
    )(
      apply(mainType, subType, charset, parameters)
    )
  }

  // based on https://github.com/square/okhttp/blob/20cd3a0/okhttp/src/main/java/okhttp3/MediaType.kt#L94
  def parse(t: String): Either[String, MediaType] = {
    val typeSubtype = Patterns.TypeSubtype.matcher(t)
    if (!typeSubtype.lookingAt()) {
      return Left(s"""No subtype found for: "$t"""")
    }
    val mainType = typeSubtype.group(1).toLowerCase
    val subType = typeSubtype.group(2).toLowerCase

    val parameters = Patterns.parseParameters(t, offset = typeSubtype.end())

    parameters match {
      case Right(params) =>
        Right(MediaType(mainType, subType, params.get("charset"), params.filter { case (p, _) => p != "charset" }))
      case l @ Left(_) => l.asInstanceOf[Either[String, MediaType]]
    }
  }

  def unsafeParse(s: String): MediaType = parse(s).getOrThrow

  private val Wildcard = "*"
}

// https://www.iana.org/assignments/media-types/media-types.xhtml
trait MediaTypes {
  val ApplicationGzip: MediaType = MediaType("application", "gzip")
  val ApplicationZip: MediaType = MediaType("application", "zip")
  val ApplicationJson: MediaType = MediaType("application", "json")
  val ApplicationOctetStream: MediaType = MediaType("application", "octet-stream")
  val ApplicationPdf: MediaType = MediaType("application", "pdf")
  val ApplicationRtf: MediaType = MediaType("application", "rtf")
  val ApplicationXhtml: MediaType = MediaType("application", "xhtml+xml")
  val ApplicationXml: MediaType = MediaType("application", "xml")
  val ApplicationXWwwFormUrlencoded: MediaType = MediaType("application", "x-www-form-urlencoded")

  val ImageGif: MediaType = MediaType("image", "gif")
  val ImageJpeg: MediaType = MediaType("image", "jpeg")
  val ImagePng: MediaType = MediaType("image", "png")
  val ImageTiff: MediaType = MediaType("image", "tiff")

  val MultipartFormData: MediaType = MediaType("multipart", "form-data")
  val MultipartMixed: MediaType = MediaType("multipart", "mixed")
  val MultipartAlternative: MediaType = MediaType("multipart", "alternative")

  val TextCacheManifest: MediaType = MediaType("text", "cache-manifest")
  val TextCalendar: MediaType = MediaType("text", "calendar")
  val TextCss: MediaType = MediaType("text", "css")
  val TextCsv: MediaType = MediaType("text", "csv")
  val TextEventStream: MediaType = MediaType("text", "event-stream")
  val TextJavascript: MediaType = MediaType("text", "javascript")
  val TextHtml: MediaType = MediaType("text", "html")
  val TextPlain: MediaType = MediaType("text", "plain")

  val TextPlainUtf8: MediaType = MediaType("text", "plain", Some("utf-8"))

  val AnyType: MediaType = MediaType("*", "*", Some("*"))
}
