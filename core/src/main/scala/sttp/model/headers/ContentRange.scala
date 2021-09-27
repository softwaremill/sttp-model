package sttp.model.headers

import sttp.model.HeaderNames
import sttp.model.internal.Validate.RichEither

import scala.util.{Failure, Success, Try}

case class ContentRange(unit: String, range: Option[(Long, Long)], size: Option[Long]) {
  override def toString: String = s"${HeaderNames.ContentRange}: $unit ${range.map(d => s"${d._1}-${d._2}").getOrElse("*")}/${size.getOrElse("*")}"
}

object ContentRange {

  def parse(str: String): Either[String, ContentRange] =
    Try(parseString(str))
      .filter(isValid) match {
      case Success(value) => Right(value)
      case Failure(exception) => Left(exception.getMessage)
    }

  private def parseString(str: String): ContentRange = {
    val splited = str.trim.split(" ")
    val unit = splited(0)
    val rangeAndSize = splited(1).split("/")
    val possibleRange = rangeAndSize(0)
    val range =
      if (possibleRange.equals("*")) None
      else {
      val longs = possibleRange.split("-").map(_.toLong)
      Some((longs(0), longs(1)))
      }
    val possibleSize: String = rangeAndSize(1)
    val size = if (possibleSize.equals("*")) None else Some(possibleSize.toLong)
    ContentRange(unit, range, size)
  }

  private def isValid(contentRange: ContentRange): Boolean = {
    val isSyntaxInvalid = contentRange.range.isEmpty && contentRange.size.isEmpty
    if (contentRange.range.isDefined) {
      val isRangeValid = contentRange.range.exists(r => r._1 < r._2)
      if (contentRange.range.isDefined && contentRange.size.isDefined) {
        val isRangeAndSizeValid = contentRange
          .range.exists(range => contentRange.size.exists(size => range._1 < size & range._2 <= size))
        isRangeValid && !isSyntaxInvalid && isRangeAndSizeValid
      } else isRangeValid && !isSyntaxInvalid
    } else !isSyntaxInvalid
  }

  def unsafeApply(s: String): ContentRange = safeApply(s).getOrThrow
  def safeApply(s: String): Either[String, ContentRange] = parse(s)
}