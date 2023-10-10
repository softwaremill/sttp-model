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
      parseAcceptHeader(headers).getOrThrow,
      parseAcceptCharsetHeader(headers).getOrThrow
    )

  private def toContentTypeRanges(
      mediaTypes: Seq[(MediaType, Float)],
      charsets: Seq[(String, Float)]
  ): Seq[ContentTypeRange] = {
    (mediaTypes, charsets) match {
      case (Nil, Nil)            => AnyRange :: Nil
      case (Nil, (ch, _) :: Nil) => ContentTypeRange(Wildcard, Wildcard, ch, emptyParameters) :: Nil
      case ((mt, _) :: Nil, Nil) => ContentTypeRange(mt.mainType, mt.subType, Wildcard, mt.otherParameters) :: Nil
      case (Nil, chs) =>
        chs.sortBy({ case (_, q) => -q }).map { case (ch, _) =>
          ContentTypeRange(Wildcard, Wildcard, ch, emptyParameters)
        }
      case (mts, Nil) =>
        mts.sortBy({ case (_, q) => -q }).map { case (mt, _) =>
          ContentTypeRange(mt.mainType, mt.subType, Wildcard, mt.otherParameters)
        }
      case (mts, chs) =>
        mts.flatMap { case (mt, mtQ) =>
          // if Accept-Charset is defined then any other charset specified in Accept header in not acceptable
          chs.map { case (ch, chQ) => (mt, ch) -> math.min(mtQ, chQ) }
        } match {
          case ((mt, ch), _) :: Nil => ContentTypeRange(mt.mainType, mt.subType, ch, mt.otherParameters) :: Nil
          case merged =>
            merged.sortBy({ case (_, q) => -q }).map { case ((mt, ch), _) =>
              ContentTypeRange(mt.mainType, mt.subType, ch, mt.otherParameters)
            }
        }
    }
  }

  private def parseAcceptHeader(headers: Seq[Header]): Either[String, Seq[(MediaType, Float)]] = {
    val errors = new java.lang.StringBuilder()
    val mts = List.newBuilder[(MediaType, Float)]
    extractEntries(headers, HeaderNames.Accept).foreach { entry =>
      MediaType.parse(entry).flatMap(mt => qValue(mt).map(mt -> _)) match {
        case Right(mt) =>
          mts += mt
        case Left(error) =>
          if (errors.length != 0) errors.append('\n')
          else ()
          errors.append(error)
      }
    }
    if (errors.length == 0) Right(mts.result())
    else Left(errors.toString)
  }

  private def parseAcceptCharsetHeader(headers: Seq[Header]): Either[String, Seq[(String, Float)]] = {
    val errors = new java.lang.StringBuilder()
    val chs = List.newBuilder[(String, Float)]
    extractEntries(headers, HeaderNames.AcceptCharset).foreach { entry =>
      parseAcceptCharsetEntry(entry) match {
        case Right(ch) => chs += ch
        case Left(error) =>
          if (errors.length != 0) errors.append('\n')
          else ()
          errors.append(error)
      }
    }
    if (errors.length == 0) Right(chs.result())
    else Left(errors.toString)
  }

  private def parseAcceptCharsetEntry(entry: String): Either[String, (String, Float)] = {
    val name = Patterns.Type.matcher(entry)
    if (name.lookingAt()) {
      Patterns
        .parseMediaTypeParameters(entry, offset = name.end())
        .flatMap(qValueFrom(_).map(name.group(1).toLowerCase -> _))
    } else Left(s"""No charset found for: "$entry"""")
  }

  private def extractEntries(headers: Seq[Header], name: String): Seq[String] = {
    val entries = List.newBuilder[String]
    headers.foreach { h =>
      if (h.is(name)) entries ++= trimInPlace(h.value.split(","))
    }
    entries.result()
  }

  private def trimInPlace(ss: Array[String]): Array[String] = {
    var i = 0
    while (i < ss.length) {
      ss(i) = ss(i).trim
      i += 1
    }
    ss
  }

  private def qValue(mt: MediaType): Either[String, Float] = qValueFrom(mt.otherParameters)

  private def qValueFrom(parameters: Map[String, String]): Either[String, Float] =
    parameters.get("q") match {
      case None => Right(1f)
      case Some(q) =>
        val qValue = Patterns.QValue.matcher(q)
        if (qValue.matches() && qValue.groupCount() == 1) Right(qValue.group(1).toFloat)
        else Left(s"""q must be numeric value between <0, 1> with up to 3 decimal points, provided "$q"""")
    }
}
