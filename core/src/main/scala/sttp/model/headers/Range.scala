package sttp.model.headers

import sttp.model.ContentRangeUnits
import sttp.model.internal.ParseUtils
import sttp.model.internal.Validate.RichEither
import scala.annotation.tailrec

case class Range(start: Option[Long], end: Option[Long], unit: String) {

  override def toString: String = {
    val sb = new java.lang.StringBuilder()
    sb.append(unit).append("=")
    start match {
      case x: Some[Long] => sb.append(x.value)
      case _             => ()
    }
    sb.append('-')
    end match {
      case x: Some[Long] => sb.append(x.value)
      case _             => ()
    }
    sb.toString
  }

  def toContentRange(fileSize: Long, unit: String = ContentRangeUnits.Bytes): ContentRange =
    ContentRange(
      unit, {
        if (start.isDefined && end.isDefined) Some((start.get, end.get))
        else None
      },
      Some(fileSize)
    )

  val contentLength: Long =
    if (end.isDefined && start.isDefined) end.get - start.get + 1
    else 0

  def isValid(contentSize: Long): Boolean =
    (start, end) match {
      case (Some(_start), Some(_end)) => _start <= _end && _end < contentSize
      case (Some(_start), None)       => _start < contentSize
      case (None, Some(_end))         => _end < contentSize
      case _                          => false
    }
}

object Range {

  def parse(str: String): Either[String, List[Range]] =
    str.split("=") match {
      case Array(unit, s) =>
        for {
          ranges <- processString(s, unit)
          _ <- if (ranges.forall(validateRange) && ranges.nonEmpty) Right(()) else Left("Invalid Range")
        } yield ranges
      case _ => Left("Expected range in the format: \"unit=start/end\", but got: %s".format(str))
    }

  private def processString(s: String, unit: String): Either[String, List[Range]] = {
    @tailrec
    def run(raw: List[String], acc: List[Range]): Either[String, List[Range]] = {
      raw match {
        case Nil => Right(acc.reverse)
        case head :: tail =>
          parseSingleRange(head, unit) match {
            case Right(r) => run(tail, r :: acc)
            case Left(e)  => Left(e)
          }
      }
    }

    run(s.split(",").toList, Nil)
  }

  private def parseSingleRange(rangeString: String, unit: String): Either[String, Range] =
    rangeString.trim.split("-") match {
      case Array(start, end) if start.trim.isEmpty =>
        for {
          e <- ParseUtils.toLongOption(end).toRight(s"Invalid end of range: $end")
        } yield Range(None, Some(e), unit)
      case Array(start, end) =>
        for {
          s <- ParseUtils.toLongOption(start).toRight(s"Invalid start of range: $start")
          e <- ParseUtils.toLongOption(end).toRight(s"Invalid end of range: $end")
        } yield Range(Some(s), Some(e), unit)
      case Array(start) =>
        for {
          s <- ParseUtils.toLongOption(start).toRight(s"Invalid start of range: $start")
        } yield Range(Some(s), None, unit)
      case _ => Right(Range(None, None, unit))
    }

  private def validateRange(range: Range): Boolean =
    (range.start, range.end) match {
      case (Some(start), Some(end)) => start <= end
      case (None, None)             => false
      case _                        => true
    }

  def unsafeParse(s: String): List[Range] = parse(s).getOrThrow

  def unsafeApply(start: Option[Long], end: Option[Long], unit: String): List[Range] =
    safeApply(start, end, unit).getOrThrow

  def safeApply(start: Option[Long], end: Option[Long], unit: String): Either[String, List[Range]] = {
    val range = Range(start, end, unit)
    if (validateRange(range)) Right(List(range))
    else Left("Invalid Range")
  }
}
