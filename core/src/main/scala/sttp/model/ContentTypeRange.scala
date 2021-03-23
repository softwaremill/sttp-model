package sttp.model

case class ContentTypeRange(mainType: String, subType: String, charset: String)

object ContentTypeRange {
  val Wildcard = "*"
  val AnyRange: ContentTypeRange = ContentTypeRange(Wildcard, Wildcard, Wildcard)
}
