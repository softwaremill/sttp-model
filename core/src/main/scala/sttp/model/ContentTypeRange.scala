package sttp.model

case class ContentTypeRange(mainType: String, subType: String, charset: String)

object ContentTypeRange {
  val Wildcard = "*"
  val AnyContentTypeRange: ContentTypeRange = ContentTypeRange(Wildcard, Wildcard, Wildcard)
}
