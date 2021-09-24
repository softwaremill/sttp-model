package sttp.model.headers

import sttp.model.internal.Validate.RichEither
import sttp.model.{ContentRangeUnits, HeaderNames}

import scala.util.Try

case class RangeHeader(start: Int, end: Int, unit: String) {

  val range: Range = Range(start, end)

  override def toString: String = s"${HeaderNames.Range} ${ContentRangeUnits.Bytes}=$start-$end"
}

object RangeHeader {

  def parse(str: String): Either[String, RangeHeader] = {
    Try(parseString(str))
      .filter(r => isValid(r.start, r.end))
      .toEither.left
      .map(_.getMessage)
  }

  private def parseString(str: String): RangeHeader = {
    val splited = str.split("=")
    val unit = splited(0)
    val range = splited(1).split("-")
    // TODO add support for other cases of range
    RangeHeader(range(0).toInt, range(1).toInt, unit)
  }

  private def isValid(start: Int, end: Int): Boolean = start < end

  def unsafeApply(s: String): RangeHeader = safeApply(s).getOrThrow

  def safeApply(s: String): Either[String, RangeHeader] = parse(s)
}

case class Range(start: Int, end: Int) {
  def isValid(contentLength: Long): Boolean = if (end < contentLength) true else false

  def toContentRange(fileSize: Long, unit: String = ContentRangeUnits.Bytes): String = unit + " " + start + "-" + end + "/" + fileSize

  val contentLength: Int = end - start
}