package sttp.model.headers

import sttp.model.headers.ETag.WeakPrefix
import sttp.model.internal.Validate.RichEither

case class ETag(tag: String, weak: Boolean = false) {
  override def toString: String = s"""${if (weak) WeakPrefix else ""}"$tag""""
}

object ETag {
  private val WeakPrefix = "W/"
  def parse(s: String): Either[String, ETag] = {
    val (s2, weak) = if (s.startsWith(WeakPrefix)) (s.substring(2), true) else (s, false)
    if (!s2.startsWith("\"") || !s2.endsWith("\"")) {
      Left("ETags must be enclosed in double quotes")
    } else {
      Right(ETag(s2.substring(1, s2.length - 1), weak))
    }
  }
  def unsafeParse(s: String): ETag = parse(s).getOrThrow
}
