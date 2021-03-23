package sttp.model.headers

import sttp.model.ContentTypeRange._
import sttp.model.internal.Patterns
import sttp.model.internal.Validate._
import sttp.model.{ContentTypeRange, Header, HeaderNames, MediaType}

import scala.collection.immutable.{Map, Seq}

object Accepts {
  def parse(headers: Seq[Header]): Either[String, Seq[ContentTypeRange]] =
    (parseAcceptHeader(headers), parseAcceptCharsetHeader(headers)) match {
      case (Right(mts), Right(chs))         => Right(toContentTypeRanges(mts, chs))
      case (Left(errorMts), Left(errorChs)) => Left(s"$errorMts\n$errorChs")
      case (Left(error), _)                 => Left(error)
      case (_, Left(error))                 => Left(error)
    }

  def unsafeParse(headers: Seq[Header]): Seq[ContentTypeRange] =
    toContentTypeRanges(
      unsafeParseAcceptHeader(headers),
      unsafeParseAcceptCharsetHeader(headers)
    )

  private def toContentTypeRanges(
      mediaTypes: Seq[(MediaType, Float)],
      charsets: Seq[(String, Float)]
  ): Seq[ContentTypeRange] = {
    (mediaTypes, charsets) match {
      case (Nil, Nil) => Seq(AnyRange)
      case (Nil, chs) =>
        chs.sortBy({ case (_, q) => -q }).map { case (ch, _) => ContentTypeRange(Wildcard, Wildcard, ch) }
      case (mts, Nil) =>
        mts.sortBy({ case (_, q) => -q }).map { case (mt, _) => ContentTypeRange(mt.mainType, mt.subType, Wildcard) }
      case (mts, chs) =>
        val merged = mts.flatMap { case (mt, mtQ) =>
          // if Accept-Charset is defined then any other charset specified in Accept header in not acceptable
          chs.map { case (ch, chQ) => (mt, ch) -> math.min(mtQ, chQ) }
        }
        merged.sortBy({ case (_, q) => -q }).map { case ((mt, ch), _) => ContentTypeRange(mt.mainType, mt.subType, ch) }
    }
  }

  private def parseAcceptHeader(headers: Seq[Header]): Either[String, Seq[(MediaType, Float)]] = {
    extractEntries(headers, HeaderNames.Accept)
      .map(MediaType.parse)
      .partition(_.isLeft) match {
      case (Nil, mts)  => Right(mts collect { case Right(mt) => mt -> qValue(mt) })
      case (errors, _) => Left(errors collect { case Left(msg) => msg } mkString "\n")
    }
  }

  private def unsafeParseAcceptHeader(headers: Seq[Header]): Seq[(MediaType, Float)] =
    extractEntries(headers, HeaderNames.Accept)
      .map { entry =>
        val mt = MediaType.unsafeParse(entry)
        mt -> qValue(mt)
      }

  private def parseAcceptCharsetHeader(headers: Seq[Header]): Either[String, Seq[(String, Float)]] =
    extractEntries(headers, HeaderNames.AcceptCharset)
      .map(parseAcceptCharsetEntry)
      .partition(_.isLeft) match {
      case (Nil, chs)  => Right(chs collect { case Right(ch) => ch })
      case (errors, _) => Left(errors collect { case Left(msg) => msg } mkString "\n")
    }

  private def unsafeParseAcceptCharsetHeader(headers: Seq[Header]): Seq[(String, Float)] =
    extractEntries(headers, HeaderNames.AcceptCharset)
      .map { entry => parseAcceptCharsetEntry(entry).getOrThrow }

  private def parseAcceptCharsetEntry(entry: String): Either[String, (String, Float)] = {
    val name = Patterns.Type.matcher(entry)
    if (!name.lookingAt()) {
      Left(s"""No charset found for: "$entry"""")
    } else {
      Patterns.parseMediaTypeParameters(entry, offset = name.end()) match {
        case Right(params) => Right(name.group(1).toLowerCase -> qValueFrom(params))
        case Left(error)   => Left(error)
      }
    }
  }

  private def extractEntries(headers: Seq[Header], name: String): Seq[String] =
    headers
      .filter(_.is(name))
      .flatMap(_.value.split(","))
      .map(_.replaceAll(Patterns.WhiteSpaces, ""))

  private def qValue(mt: MediaType): Float = qValueFrom(mt.otherParameters)

  private def qValueFrom(parameters: Map[String, String]): Float =
    (parameters.get("q") collect { case Patterns.QValue(q) => q.toFloat }).getOrElse(1f)
}
