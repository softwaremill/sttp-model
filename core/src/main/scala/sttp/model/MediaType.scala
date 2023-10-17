package sttp.model

import sttp.model.ContentTypeRange.Wildcard
import sttp.model.internal.Patterns
import sttp.model.internal.Rfc2616._
import sttp.model.internal.Validate._

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

  // #2994 from tapir: when the media type doesn't define a charset, it shouldn't be taken into account in the matching logic
  def matches(range: ContentTypeRange): Boolean =
    range != null &&
      (range.mainType == Wildcard || mainType.equalsIgnoreCase(range.mainType) &&
        (range.subType == Wildcard || subType.equalsIgnoreCase(range.subType))) &&
      (range.charset == Wildcard || charset.forall(_.equalsIgnoreCase(range.charset))) &&
      (range.mainType == Wildcard || range.subType == Wildcard || otherParameters.isEmpty || {
        // `otherParameters` needs to be fully contained within `range.otherParameters` (ignoring case), but only if the main/sub type aren't wildcards
        val rangeOtherParametersLowerCased = range.otherParameters.map(x => (x._1.toLowerCase, x._2.toLowerCase))
        otherParametersLowerCased.forall { case (k, v) =>
          rangeOtherParametersLowerCased.get(k).contains(v)
        }
      })

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

  override def toString: String = {
    val sb = new java.lang.StringBuilder(32) // "application/json; charset=utf-8".length == 31 ;)
    sb.append(mainType).append('/').append(subType)
    charset match {
      case x: Some[String] => sb.append("; charset=").append(x.value)
      case _               => ()
    }
    otherParameters.foreach { case (p, v) =>
      if (p != "charset") sb.append("; ").append(p).append('=').append(v)
      else ()
    }
    sb.toString
  }

  override lazy val hashCode: Int = toString.toLowerCase.hashCode

  override def equals(that: Any): Boolean =
    that match {
      case t: AnyRef if this.eq(t) => true
      case t: MediaType            => toString.equalsIgnoreCase(t.toString)
      case _                       => false
    }

  def equalsIgnoreParameters(that: MediaType): Boolean =
    mainType.equalsIgnoreCase(that.mainType) && subType.equalsIgnoreCase(that.subType)

  private val otherParametersLowerCased: Map[String, String] =
    otherParameters.map(x => (x._1.toLowerCase, x._2.toLowerCase))
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
  ): Either[String, MediaType] =
    validateToken("Main type", mainType)
      .orElse(validateToken("Sub type", subType))
      .orElse(charset.flatMap(validateToken("Charset", _)))
      .orElse(parameters.collectFirst {
        case (p, v) if validateToken(p, v).isDefined => validateToken(p, v).get
      }) match {
      case None        => Right(apply(mainType, subType, charset, parameters))
      case Some(error) => Left(error)
    }

  // based on https://github.com/square/okhttp/blob/20cd3a0/okhttp/src/main/java/okhttp3/MediaType.kt#L94
  def parse(t: String): Either[String, MediaType] = {
    val typeSubtype = Patterns.TypeSubtype.matcher(t)
    if (typeSubtype.lookingAt()) {
      val (mainType, subType) = (typeSubtype.group(1), typeSubtype.group(2), typeSubtype.group(3)) match {
        // if there are nulls indicating no main and subtype then we expect a single * (group 3)
        // it's invalid according to rfc but is used by `HttpUrlConnection` https://bugs.openjdk.java.net/browse/JDK-8163921
        case (null, null, Wildcard) => (Wildcard, Wildcard)
        case (mainType, subType, _) => (mainType.toLowerCase, subType.toLowerCase)
      }
      Patterns
        .parseMediaTypeParameters(t, offset = typeSubtype.end())
        .map(params => MediaType(mainType, subType, params.get("charset"), params - "charset"))
    } else Left(s"""No subtype found for: "$t"""")
  }

  def unsafeParse(s: String): MediaType = parse(s).getOrThrow

  /** @param mediaTypes
    *   Candidate media types
    * @param ranges
    *   Content type ranges, sorted in order of preference.
    */
  def bestMatch(mediaTypes: Seq[MediaType], ranges: Seq[ContentTypeRange]): Option[MediaType] = {
    var minMt: MediaType = null
    var minIndex = Int.MaxValue
    mediaTypes.foreach { mt =>
      val index = ranges.indexWhere(mt.matches)
      if (index >= 0 && minIndex > index) {
        minIndex = index
        minMt = mt
      }
    }
    Option(minMt)
  }
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
