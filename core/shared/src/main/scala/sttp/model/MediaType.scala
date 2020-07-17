package sttp.model

import java.nio.charset.Charset

import internal.Validate._
import java.util.regex.Pattern

import sttp.model.internal.Validate
import sttp.model.internal.Rfc2616._

case class MediaType(mainType: String, subType: String, charset: Option[String] = None) {
  def charset(c: Charset): MediaType = charset(c.name())
  def charset(c: String): MediaType = copy(charset = Some(c))
  def noCharset: MediaType = copy(charset = None)

  override def toString: String = s"$mainType/$subType" + charset.fold("")(c => s"; charset=$c")
}

/**
  * For a description of the behavior of `apply`, `parse`, `safeApply` and `unsafeApply` methods, see [[sttp.model]].
  */
object MediaType extends MediaTypes {
  /**
    * @throws IllegalArgumentException If the main type or subt type contain illegal characters.
    */
  def unsafeApply(mainType: String, subType: String, charset: Option[String] = None): MediaType =
    safeApply(mainType, subType, charset).getOrThrow
  def safeApply(mainType: String, subType: String, charset: Option[String] = None): Either[String, MediaType] = {
    Validate.all(
      validateToken("Main type", mainType),
      validateToken("Sub type", subType),
      charset.flatMap(validateToken("Charset", _))
    )(
      apply(mainType, subType, charset)
    )
  }
  @deprecated("use apply)")
  def notValidated(mainType: String, subType: String, charset: Option[String] = None): MediaType =
    apply(mainType, subType, charset)

  // based on https://github.com/square/okhttp/blob/20cd3a0/okhttp/src/main/java/okhttp3/MediaType.kt#L94
  private val TOKEN = "([a-zA-Z0-9-!#$%&'*+.^_`{|}~]+)"
  private val QUOTED = "\"([^\"]*)\""
  private val TYPE_SUBTYPE = Pattern.compile(s"$TOKEN/$TOKEN")
  private val PARAMETER = Pattern.compile(s";\\s*(?:$TOKEN=(?:$TOKEN|$QUOTED))?")

  def parse(t: String): Either[String, MediaType] = {
    val typeSubtype = TYPE_SUBTYPE.matcher(t)
    if (!typeSubtype.lookingAt()) {
      return Left(s"""No subtype found for: "$t"""")
    }
    val mainType = typeSubtype.group(1).toLowerCase
    val subType = typeSubtype.group(2).toLowerCase

    var charset: Option[String] = None
    val parameter = PARAMETER.matcher(t)
    var s = typeSubtype.end()
    val length = t.length
    while (s < length) {
      parameter.region(s, length)
      if (!parameter.lookingAt()) {
        return Left(s"""Parameter is not formatted correctly: \"${t.substring(s)}\" for: \"$t\"""")
      }

      val name = parameter.group(1)
      if (name == null || !name.equalsIgnoreCase("charset")) {
        s = parameter.end()
      } else {
        val token = parameter.group(2)
        val charsetParameter = token match {
          case null =>
            // Value is "double-quoted". That's valid and our regex group already strips the quotes.
            parameter.group(3)
          case _ if token.startsWith("'") && token.endsWith("'") && token.length > 2 =>
            // If the token is 'single-quoted' it's invalid! But we're lenient and strip the quotes.
            token.substring(1, token.length - 1)
          case _ => token
        }
        if (charset.nonEmpty && charset.forall(charsetParameter.equalsIgnoreCase)) {
          return Left(s"""Multiple charsets defined: \"$charset\" and: \"$charsetParameter\" for: \"$t\"""")
        }
        charset = Option(charsetParameter)
        s = parameter.end()
      }
    }

    Right(MediaType(mainType, subType, charset))
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
