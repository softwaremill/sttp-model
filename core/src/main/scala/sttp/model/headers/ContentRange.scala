package sttp.model.headers

import sttp.model.internal.Validate.RichEither
import sttp.model.{ContentRangeUnits, HeaderNames}

import scala.util.Try

case class ContentRange(unit: String, range: Option[(Long, Long)], size: Option[Long]) {

  override def toString: String = s"${HeaderNames.ContentRange}: $unit ${range.map(d => s"${d._1}-${d._2}").getOrElse("*")}/${size.getOrElse("*")}"

}

object ContentRange {

  def parse(str: String): Either[String, ContentRange] = {
    Try(parseString(str))
      .filter(isValid)
      .toEither.left
      .map(_.getMessage)
  }

  private def parseString(str: String): ContentRange = {
    val splited = str.trim.split(" ")
    val unit = splited(0)
    val rangeAndSize = splited(1).split("/")
    val possibleRange = rangeAndSize(0)
    val range = if (possibleRange.equals("*")) None else {
      val longs = possibleRange.split("-").map(_.toLong)
      Some(longs(0), longs(1))
    }
    val possibleSize: String = rangeAndSize(1)
    val size = if (possibleSize.equals("*")) None else Some(possibleSize.toLong)
    ContentRange(unit, range, size)
  }

  private def isValid(contentRange: ContentRange): Boolean = {
    val isRangeValid = contentRange.range.exists(d => d._1 < d._2)
    val isSyntaxInvalid = contentRange.range.isEmpty && contentRange.size.isEmpty
    val isRangeAndSizeValid = contentRange.range.flatMap(range => contentRange.size.map(size => range._1 < size & range._2 <= size)).getOrElse(false)
    if (isRangeValid && !isSyntaxInvalid && isRangeAndSizeValid) true
    else false
  }

  def unsafeApply(s: String): ContentRange = safeApply(s).getOrThrow

  def safeApply(s: String): Either[String, ContentRange] = parse(s)
}