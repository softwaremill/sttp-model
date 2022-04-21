package sttp.model

import sttp.model.ContentTypeRange.Wildcard

case class ContentTypeRange(mainType: String, subType: String, charset: String) {
  def anyCharset: ContentTypeRange = copy(charset = Wildcard)
  def anySubType: ContentTypeRange = copy(subType = Wildcard)

  override def toString: String = s"$mainType/$subType" + (if (charset == Wildcard) "" else s"; charset=$charset")

  override def hashCode(): Int = toString.toLowerCase.hashCode
  override def equals(that: Any): Boolean =
    that match {
      case t: AnyRef if this.eq(t) => true
      case t: ContentTypeRange     => toString.equalsIgnoreCase(t.toString)
      case _                       => false
    }
}

object ContentTypeRange {
  val Wildcard = "*"
  val AnyRange: ContentTypeRange = ContentTypeRange(Wildcard, Wildcard, Wildcard)
  val AnyApplication: ContentTypeRange = ContentTypeRange("application", Wildcard, Wildcard)
  val AnyAudio: ContentTypeRange = ContentTypeRange("audio", Wildcard, Wildcard)
  val AnyImage: ContentTypeRange = ContentTypeRange("image", Wildcard, Wildcard)
  val AnyMessage: ContentTypeRange = ContentTypeRange("message", Wildcard, Wildcard)
  val AnyMultipart: ContentTypeRange = ContentTypeRange("multipart", Wildcard, Wildcard)
  val AnyText: ContentTypeRange = ContentTypeRange("text", Wildcard, Wildcard)
  val AnyVideo: ContentTypeRange = ContentTypeRange("video", Wildcard, Wildcard)
  val AnyFont: ContentTypeRange = ContentTypeRange("font", Wildcard, Wildcard)
  val AnyExample: ContentTypeRange = ContentTypeRange("example", Wildcard, Wildcard)
  val AnyModel: ContentTypeRange = ContentTypeRange("model", Wildcard, Wildcard)

  def exact(mt: MediaType): ContentTypeRange = ContentTypeRange(mt.mainType, mt.subType, mt.charset.getOrElse(Wildcard))
  def exactNoCharset(mt: MediaType): ContentTypeRange = ContentTypeRange(mt.mainType, mt.subType, Wildcard)
}
