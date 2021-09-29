package sttp.model.headers

import sttp.model.HeaderNames
import sttp.model.internal.Validate.RichEither

case class ContentRange(unit: String, range: Option[(Long, Long)], size: Option[Long]) {
  override def toString: String =
    s"${HeaderNames.ContentRange}: $unit ${range.map(r => s"${r._1}-${r._2}").getOrElse("*")}/${size.getOrElse("*")}"
}

object ContentRange {

  def parse(str: String): Either[String, ContentRange] =
    str.trim.split(" ") match {
      case Array(unit, s) =>
        s.split("/") match {
          case Array(possibleRange, possibleSize) => processString(unit, possibleRange, possibleSize)
          case _                                  => Left("Unable to parse incorrect string: %s".format(s))
        }
      case _ => Left("Unable to parse incorrect string: %s".format(str))
    }

  private def processString(unit: String, possibleRange: String, possibleSize: String): Either[String, ContentRange] = {
    val range = possibleRange.split("-") match {
      case Array("*")        => None
      case Array(start, end) => Some((start.toLong, end.toLong))
      case _                 => None
    }
    val size = if (possibleSize.equals("*")) None else Some(possibleSize.toLong)
    val contentRange = ContentRange(unit, range, size)
    if (isValid(contentRange)) Right(contentRange)
    else Left("Invalid Content-Range")
  }

  private def isValid(contentRange: ContentRange): Boolean = {
    val isSyntaxInvalid = contentRange.range.isEmpty && contentRange.size.isEmpty
    if (contentRange.range.isDefined) {
      val isRangeValid = contentRange.range.exists(r => r._1 < r._2)
      if (contentRange.range.isDefined && contentRange.size.isDefined) {
        val isRangeAndSizeValid =
          contentRange.range.exists(range => contentRange.size.exists(size => range._1 < size & range._2 <= size))
        isRangeValid && !isSyntaxInvalid && isRangeAndSizeValid
      } else isRangeValid && !isSyntaxInvalid
    } else !isSyntaxInvalid
  }

  def unsafeParse(s: String): ContentRange = parse(s).getOrThrow

  def unsafeApply(unit: String, range: Option[(Long, Long)], size: Option[Long]): ContentRange =
    safeApply(unit, range, size).getOrThrow

  def safeApply(unit: String, range: Option[(Long, Long)], size: Option[Long]): Either[String, ContentRange] = {
    val contentRange = ContentRange(unit, range, size)
    if (isValid(contentRange)) Right(contentRange)
    else Left("Invalid Content Range")
  }
}