package sttp.model.headers

import sttp.model.internal.ParseUtils
import sttp.model.internal.Validate.RichEither
import sttp.model.{ContentRangeUnits, HeaderNames}

import scala.annotation.tailrec

case class Range(start: Option[Long], end: Option[Long], unit: String) {

  val range: RangeValue = RangeValue(start, end)

  override def toString: String = s"${HeaderNames.Range}: $unit=${start.getOrElse("")}-${end.getOrElse("")}"
}

object Range {

  def parse(str: String): Either[String, List[Range]] = {
    str.split("=") match {
      case Array(unit, s) =>
        val ranges = processString(s, unit, List.empty)
        if (ranges.forall(isValid) && ranges.nonEmpty) Right(ranges.reverse)
        else Left("Invalid Range")
      case _ => Left("Expected range in the format: \"unit=start/end\", but got: %s".format(str))
    }
  }

  @tailrec
  private def processString(v2: String, unit: String, acc: List[Range]): List[Range] = {
    v2.split(",").toList match {
      case x :: tail if x.nonEmpty =>
        val range = parsSingleRange(x, unit)
        processString(tail.mkString(","), unit, range :: acc)
      case Nil => List(parsSingleRange(v2, unit))
      case _   => acc
    }
  }

  private def parsSingleRange(rangeString: String, unit: String): Range =
    rangeString.trim.split("-") match {
      case Array(start, end) => Range(ParseUtils.toLongOption(start), ParseUtils.toLongOption(end), unit)
      case Array(start)      => Range(ParseUtils.toLongOption(start), None, unit)
      case _                 => Range(None, None, unit)
    }

  private def isValid(range: Range): Boolean =
    (range.start, range.end) match {
      case (Some(start), Some(end)) => start < end
      case (Some(_), None)          => true
      case (None, Some(_))          => true
      case _                        => false
    }

  def unsafeParse(s: String): List[Range] = parse(s).getOrThrow

  def unsafeApply(start: Option[Long], end: Option[Long], unit: String): List[Range] =
    safeApply(start, end, unit).getOrThrow

  def safeApply(start: Option[Long], end: Option[Long], unit: String): Either[String, List[Range]] = {
    val range = Range(start, end, unit)
    if (isValid(range)) Right(List(range))
    else Left("Invalid Range")
  }
}

case class RangeValue(start: Option[Long], end: Option[Long]) {
  def isValid(contentLength: Long): Boolean = if (end.exists(e => e < contentLength)) true else false

  def toContentRange(fileSize: Long, unit: String = ContentRangeUnits.Bytes): ContentRange =
    ContentRange(unit, start.zip(end).headOption, Some(fileSize))

  val contentLength: Long = end.zip(start).map(r => r._1 - r._2).headOption.getOrElse(0)
}
