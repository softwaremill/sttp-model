package sttp.model.headers

import sttp.model.internal.Patterns
import sttp.model.internal.Validate._
import sttp.model.{Header, HeaderNames, MediaType}

import scala.collection.immutable.{Map, Seq}

private[model] object Accepts {
  def unsafeParse(headers: Seq[Header]): Seq[MediaType] = {
    val mediaTypes = parseAcceptHeader(headers)
    val charsets = parseAcceptCharsetHeader(headers)

    (mediaTypes, charsets) match {
      case (Nil, Nil) => Seq(MediaType.AnyType)
      case (Nil, chs) => chs.sortBy({ case (_, q) => -q }).map { case (ch, _) => MediaType.AnyType.charset(ch) }
      case (mts, Nil) => mts.sortBy({ case (_, q) => -q }).map { case (mt, _) => mt }
      case (mts, chs) =>
        val merged = mts.flatMap { case (mt, mtQ) =>
          // if Accept-Charset is defined then any other charset specified in Accept header in not acceptable
          chs.map { case (ch, chQ) => mt.charset(ch) -> math.min(mtQ, chQ) }
        }
        merged.sortBy({ case (_, q) => -q }).map { case (mt, _) => mt }
    }
  }

  private def parseAcceptHeader(headers: Seq[Header]): Seq[(MediaType, Float)] =
    extractEntries(headers, HeaderNames.Accept)
      .map { entry =>
        val mt = MediaType.unsafeParse(entry)
        mt -> qValue(mt)
      }

  private def parseAcceptCharsetHeader(headers: Seq[Header]): Seq[(String, Float)] =
    extractEntries(headers, HeaderNames.AcceptCharset)
      .map { entry =>
        val name = Patterns.Type.matcher(entry)
        if (!name.lookingAt()) {
          throw new IllegalArgumentException(s"""No charset found for: "$entry"""")
        }
        val parameters = Patterns.parseParameters(entry, offset = name.end()).getOrThrow
        (name.group(1).toLowerCase, qValueFrom(parameters))
      }

  private def extractEntries(headers: Seq[Header], name: String): Seq[String] =
    headers
      .filter(_.is(name))
      .flatMap(_.value.split(","))
      .map(_.replaceAll(Patterns.WhiteSpaces, ""))

  private def qValue(mt: MediaType): Float = qValueFrom(mt.parameters)

  private def qValueFrom(parameters: Map[String, String]): Float =
    (parameters.get("q") collect { case Patterns.QValue(q) => q.toFloat }).getOrElse(1f)
}
