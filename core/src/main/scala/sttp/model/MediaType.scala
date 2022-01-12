package sttp.model

import sttp.model.ContentTypeRange.Wildcard
import sttp.model.internal.Rfc2616._
import sttp.model.internal.Validate._
import sttp.model.internal.{Patterns, Validate}

import java.nio.charset.Charset

case class MediaType(
    mainType: String,
    subType: String,
    charset: Option[String] = None,
    otherParameters: Map[String, String] = Map.empty
) {
  def charset(c: Charset): MediaType = charset(c.name())
  def charset(c: String): MediaType = copy(charset = Some(c))
  def noCharset: MediaType = copy(charset = None)

  def matches(range: ContentTypeRange): Boolean = {
    def charsetMatches: Boolean =
      if (range.charset == Wildcard) true
      else this.charset.map(_.toLowerCase).contains(range.charset.toLowerCase)

    (range match {
      case ContentTypeRange(Wildcard, _, _)        => true
      case ContentTypeRange(mainType, Wildcard, _) => this.mainType.equalsIgnoreCase(mainType)
      case ContentTypeRange(mainType, subType, _) =>
        this.mainType.equalsIgnoreCase(mainType) && this.subType.equalsIgnoreCase(subType)
      case null => false
    }) && charsetMatches
  }

  def isApplication: Boolean = mainType.equalsIgnoreCase("application")
  def isAudio: Boolean = mainType.equalsIgnoreCase("audio")
  def isImage: Boolean = mainType.equalsIgnoreCase("image")
  def isMessage: Boolean = mainType.equalsIgnoreCase("message")
  def isMultipart: Boolean = mainType.equalsIgnoreCase("multipart")
  def isText: Boolean = mainType.equalsIgnoreCase("text")
  def isVideo: Boolean = mainType.equalsIgnoreCase("video")
  def isFont: Boolean = mainType.equalsIgnoreCase("font")
  def isExample: Boolean = mainType.equalsIgnoreCase("example")
  def isModel: Boolean = mainType.equalsIgnoreCase("model")

  override def toString: String = s"$mainType/$subType" + charset.fold("")(c => s"; charset=$c") +
    otherParameters.foldLeft("") { case (s, (p, v)) => if (p == "charset") s else s"$s; $p=$v" }

  override def hashCode(): Int = toString.toLowerCase.hashCode
  override def equals(that: Any): Boolean =
    that match {
      case t: AnyRef if this.eq(t) => true
      case t: MediaType            => toString.equalsIgnoreCase(t.toString)
      case _                       => false
    }

  def equalsIgnoreParameters(that: MediaType): Boolean =
    mainType.equalsIgnoreCase(that.mainType) && subType.equalsIgnoreCase(that.subType)
}

/** For a description of the behavior of `apply`, `parse`, `safeApply` and `unsafeApply` methods, see [[sttp.model]]. */
object MediaType extends MediaTypes {

  /** @throws IllegalArgumentException
    *   If the main type or subt type contain illegal characters.
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

    val (mainType, subType) = (typeSubtype.group(1), typeSubtype.group(2), typeSubtype.group(3)) match {
      // if there are nulls indicating no main and subtype then we expect a single * (group 3)
      // it's invalid according to rfc but is used by `HttpUrlConnection` https://bugs.openjdk.java.net/browse/JDK-8163921
      case (null, null, Wildcard) => (Wildcard, Wildcard)
      case (mainType, subType, _) => (mainType.toLowerCase, subType.toLowerCase)
    }

    val parameters = Patterns.parseMediaTypeParameters(t, offset = typeSubtype.end())

    parameters match {
      case Right(params) =>
        Right(MediaType(mainType, subType, params.get("charset"), params.filter { case (p, _) => p != "charset" }))
      case Left(error) => Left(error)
    }
  }

  def unsafeParse(s: String): MediaType = parse(s).getOrThrow

  /** @param mediaTypes
    *   Candidate media types
    * @param ranges
    *   Content type ranges, sorted in order of preference.
    */
  def bestMatch(mediaTypes: Seq[MediaType], ranges: Seq[ContentTypeRange]): Option[MediaType] = {
    mediaTypes
      .map(mt => mt -> ranges.indexWhere(mt.matches))
      .filter({ case (_, i) => i != NotFoundIndex }) // not acceptable
    match {
      case Nil => None
      case mts => Some(mts.minBy({ case (_, i) => i })).map { case (mt, _) => mt }
    }
  }

  private val NotFoundIndex = -1
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
}
