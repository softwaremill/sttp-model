package sttp.model.headers

import sttp.model.ContentRangeUnits
import sttp.model.internal.ParseUtils
import sttp.model.internal.Validate.RichEither

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
        val ranges = processString(s, unit)
        if (ranges.forall(validateRange) && ranges.nonEmpty) Right(ranges.reverse)
        else Left("Invalid Range")
      case _ => Left("Expected range in the format: \"unit=start/end\", but got: %s".format(str))
    }

  private def processString(s: String, unit: String): List[Range] =
    s.split(",").map(parseSingleRange(_, unit)).reverse.toList // TODO: do we need `.reverse` here yet?

  private def parseSingleRange(rangeString: String, unit: String): Range =
    rangeString.trim.split("-") match {
      case Array(start, end) => Range(ParseUtils.toLongOption(start), ParseUtils.toLongOption(end), unit)
      case Array(start)      => Range(ParseUtils.toLongOption(start), None, unit)
      case _                 => Range(None, None, unit)
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
