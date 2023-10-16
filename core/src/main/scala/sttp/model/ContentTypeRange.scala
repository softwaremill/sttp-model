package sttp.model

import sttp.model.ContentTypeRange.{Wildcard, EmptyParameters}

case class ContentTypeRange(mainType: String, subType: String, charset: String, otherParameters: Map[String, String]) {
  // required for binary compatibility
  def this(mainType: String, subType: String, charset: String) = this(mainType, subType, charset, EmptyParameters)

  def copy(
      mainType: String = this.mainType,
      subType: String = this.subType,
      charset: String = this.charset,
      otherParameters: Map[String, String] = this.otherParameters
  ): ContentTypeRange =
    ContentTypeRange(mainType, subType, charset, otherParameters)

  // required for binary compatibility
  def copy(mainType: String, subType: String, charset: String): ContentTypeRange =
    ContentTypeRange(mainType, subType, charset, this.otherParameters)

  def anyCharset: ContentTypeRange = copy(charset = Wildcard)

  def anySubType: ContentTypeRange = copy(subType = Wildcard)

  override def toString: String = {
    val sb = new java.lang.StringBuilder(32) // "application/json; charset=utf-8".length == 31 ;)
    sb.append(mainType).append('/').append(subType)
    if (charset != Wildcard) sb.append("; charset=").append(charset)
    otherParameters.foreach { case (p, v) =>
      if (p != "charset") sb.append("; ").append(p).append('=').append(v)
      else ()
    }
    sb.toString
  }

  override def hashCode(): Int = toString.toLowerCase.hashCode

  override def equals(that: Any): Boolean =
    that match {
      case t: AnyRef if this.eq(t) => true
      case t: ContentTypeRange     => toString.equalsIgnoreCase(t.toString)
      case _                       => false
    }
}

object ContentTypeRange {
  // required for binary compatibility
  def apply(mainType: String, subType: String, charset: String): ContentTypeRange =
    new ContentTypeRange(mainType, subType, charset, EmptyParameters)

  val Wildcard = "*"
  val EmptyParameters: Map[String, String] = Map.empty

  val AnyRange: ContentTypeRange = ContentTypeRange(Wildcard, Wildcard, Wildcard, EmptyParameters)
  val AnyApplication: ContentTypeRange = ContentTypeRange("application", Wildcard, Wildcard, EmptyParameters)
  val AnyAudio: ContentTypeRange = ContentTypeRange("audio", Wildcard, Wildcard, EmptyParameters)
  val AnyImage: ContentTypeRange = ContentTypeRange("image", Wildcard, Wildcard, EmptyParameters)
  val AnyMessage: ContentTypeRange = ContentTypeRange("message", Wildcard, Wildcard, EmptyParameters)
  val AnyMultipart: ContentTypeRange = ContentTypeRange("multipart", Wildcard, Wildcard, EmptyParameters)
  val AnyText: ContentTypeRange = ContentTypeRange("text", Wildcard, Wildcard, EmptyParameters)
  val AnyVideo: ContentTypeRange = ContentTypeRange("video", Wildcard, Wildcard, EmptyParameters)
  val AnyFont: ContentTypeRange = ContentTypeRange("font", Wildcard, Wildcard, EmptyParameters)
  val AnyExample: ContentTypeRange = ContentTypeRange("example", Wildcard, Wildcard, EmptyParameters)
  val AnyModel: ContentTypeRange = ContentTypeRange("model", Wildcard, Wildcard, EmptyParameters)

  def exact(mt: MediaType): ContentTypeRange =
    ContentTypeRange(mt.mainType, mt.subType, mt.charset.getOrElse(Wildcard), mt.otherParameters)

  def exactNoCharset(mt: MediaType): ContentTypeRange =
    ContentTypeRange(mt.mainType, mt.subType, Wildcard, mt.otherParameters)
}
