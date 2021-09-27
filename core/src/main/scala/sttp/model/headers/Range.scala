package sttp.model.headers

import sttp.model.internal.Validate.RichEither
import sttp.model.{ContentRangeUnits, HeaderNames}

import scala.util.{Failure, Success, Try}

case class Range(start: Option[Long], end: Option[Long], unit: String) {

  val range: RangeValue = RangeValue(start, end)

  override def toString: String = s"${HeaderNames.Range}: $unit=${start.getOrElse("")}-${end.getOrElse("")}"
}

object Range {

  def parse(str: String): Either[String, List[Range]] =
    Try(parseString(str))
    .filter(_.forall(isValid)) match {
      case Success(value) => Right(value)
      case Failure(exception) => Left(exception.getMessage)
    }

  private def parseString(str: String): List[Range] = {
    val splited = str.split("=")
    val unit = splited(0)
    if (splited(1).contains(",")) splited(1).split(",").map(s => parsSingleRange(s, unit)).toList
    else List(parsSingleRange(splited(1), unit))
  }

  private def parsSingleRange(rangeString: String, unit: String): Range = {
    val strings = rangeString.trim.split("-")
    if (strings.size == 2) Range(toLongOption(strings(0)), toLongOption(strings(1)), unit)
    else Range(toLongOption(strings(0)), None, unit)
  }

  private def toLongOption(s: String): Option[Long] = {
    if(s.isEmpty) None
    else Some(s.toLong)
  }

  private def isValid(range: Range): Boolean = {
    val start = range.start
    val end = range.end
    val isCorrectlyDefined = start.isDefined || end.isDefined
    if (start.isDefined && end.isDefined) {
      val isRangeValid = start.zip(end).exists(startToEnd => startToEnd._1 < startToEnd._2)
      isCorrectlyDefined && isRangeValid
    } else isCorrectlyDefined
  }

  def unsafeApply(s: String): List[Range] = safeApply(s).getOrThrow
  def safeApply(s: String): Either[String, List[Range]] = parse(s)
}

case class RangeValue(start: Option[Long], end: Option[Long]) {
  def isValid(contentLength: Long): Boolean = if (end.exists(e => e < contentLength)) true else false

  def toContentRange(fileSize: Long, unit: String = ContentRangeUnits.Bytes): String = unit + " " + start.getOrElse(0) + "-" + end.getOrElse(0) + "/" + fileSize

  val contentLength: Long = end.zip(start).map(r => r._1 - r._2).headOption.getOrElse(0)
}