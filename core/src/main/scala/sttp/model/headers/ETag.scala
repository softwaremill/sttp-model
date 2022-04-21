package sttp.model.headers

import sttp.model.headers.ETag.WeakPrefix
import sttp.model.internal.Validate.RichEither

import scala.annotation.tailrec

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
  def parseList(s: String): Either[String, List[ETag]] = {
    val chars = s.toCharArray
    val count = chars.length
    @tailrec
    def run(i: Int, start: Int, inQuotes: Boolean, acc: List[ETag]): Either[String, List[ETag]] = {
      def next(): Either[String, ETag] = parse(s.substring(start, i).trim)

      if (i >= count) {
        next() match {
          case Left(e)  => Left(e)
          case Right(e) => Right((e :: acc).reverse)
        }
      } else if (inQuotes) {
        if (chars(i) == '"') {
          run(i + 1, start, inQuotes = false, acc)
        } else {
          run(i + 1, start, inQuotes, acc)
        }
      } else
        chars(i) match {
          case '"' => run(i + 1, start, inQuotes = true, acc)
          case ',' =>
            next() match {
              case Left(e)  => Left(e)
              case Right(e) => run(i + 1, i + 1, inQuotes = false, e :: acc)
            }
          case c => run(i + 1, start, inQuotes = false, acc)
        }
    }
    run(0, 0, inQuotes = false, Nil)
  }

  def unsafeParse(s: String): ETag = parse(s).getOrThrow
  def unsafeParseList(s: String): List[ETag] = parseList(s).getOrThrow

  /** @return Representation of the ETags as in a header value, in the format: `"etag1", "etag2", ...`. */
  def toString(es: List[ETag]): String = es.map(_.toString).mkString(", ")
}
