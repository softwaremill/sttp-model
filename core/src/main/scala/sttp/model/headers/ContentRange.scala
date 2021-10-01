package sttp.model.headers

import sttp.model.HeaderNames
import sttp.model.internal.ParseUtils
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
          case _ => Left("Expected string in the format: \"range/size\", but got: %s".format(s))
        }
      case _ => Left("Expected content-range in the format: \"unit range/size\", but got: %s".format(str))
    }

  private def processString(unit: String, possibleRange: String, possibleSize: String): Either[String, ContentRange] = {
    val range = possibleRange.split("-") match {
      case Array("*")        => None
      case Array(start, end) => ParseUtils.toLongOption(start).zip(ParseUtils.toLongOption(end)).headOption
      case _                 => None
    }
    val size = if (possibleSize.equals("*")) None else ParseUtils.toLongOption(possibleSize)
    val contentRange = ContentRange(unit, range, size)
    if (isValid(contentRange)) Right(contentRange)
    else Left("Invalid Content-Range")
  }

  private def isValid(contentRange: ContentRange): Boolean = {
    val isSyntaxInvalid = contentRange.range.isEmpty && contentRange.size.isEmpty
    contentRange.range match {
      case Some(range) =>
        val isRangeValid = range._1 < range._2
        contentRange.size match {
          case Some(size) =>
            val isRangeAndSizeValid = range._1 < size & range._2 <= size
            isRangeValid && !isSyntaxInvalid && isRangeAndSizeValid
          case None => isRangeValid && !isSyntaxInvalid
        }
      case None => !isSyntaxInvalid
    }
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
