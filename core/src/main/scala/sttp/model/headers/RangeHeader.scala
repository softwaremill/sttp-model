package sttp.model.headers

import sttp.model.internal.Validate.RichEither
import sttp.model.{ContentRangeUnits, HeaderNames}

import scala.util.{Failure, Success, Try}

case class RangeHeader(start: Option[Int], end: Option[Int], unit: String) {

  val range: Range = Range(start, end)

  override def toString: String = s"${HeaderNames.Range}: $unit=${start.getOrElse(0)}-${end.getOrElse(0)}"
}

object RangeHeader {

  def parse(str: String): Either[String, List[RangeHeader]] =
    Try(parseString(str))
    .filter(_.forall(isValid)) match {
      case Success(value) => Right(value)
      case Failure(exception) => Left(exception.getMessage)
    }

  private def parseString(str: String): List[RangeHeader] = {
    val splited = str.split("=")
    val unit = splited(0)
    if (splited(1).contains(",")) splited(1).split(",").map(s => parsSingleRange(s, unit)).toList
    else List(parsSingleRange(splited(1), unit))
  }

  private def parsSingleRange(rangeString: String, unit: String): RangeHeader = {
    val strings = rangeString.trim.split("-")
    if (strings.size == 2) RangeHeader(toIntOption(strings(0)), toIntOption(strings(1)), unit)
    else RangeHeader(toIntOption(strings(0)), None, unit)
  }

  private def toIntOption(s: String): Option[Int] = {
    if(s != null) None
    else Some(s.toInt)
  }

  private def isValid(range: RangeHeader): Boolean = {
    val start = range.start
    val end = range.end
    val isCorrectlyDefined = start.isDefined || end.isDefined
    if (start.isDefined && end.isDefined) {
      val isRangeValid = start.zip(end).exists(startToEnd => startToEnd._1 < startToEnd._2)
      isCorrectlyDefined && isRangeValid
    } else isCorrectlyDefined
  }

  def unsafeApply(s: String): List[RangeHeader] = safeApply(s).getOrThrow
  def safeApply(s: String): Either[String, List[RangeHeader]] = parse(s)
}

case class Range(start: Option[Int], end: Option[Int]) {
  def isValid(contentLength: Long): Boolean = if (end.exists(e => e < contentLength)) true else false

  def toContentRange(fileSize: Long, unit: String = ContentRangeUnits.Bytes): String = unit + " " + start.getOrElse(0) + "-" + end.getOrElse(0) + "/" + fileSize

  val contentLength: Int = end.zip(start).map(r => r._1 - r._2).headOption.getOrElse(0)
}