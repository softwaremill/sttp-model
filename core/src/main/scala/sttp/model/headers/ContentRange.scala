package sttp.model.headers

import sttp.model.internal.ParseUtils
import sttp.model.internal.Validate.RichEither

case class ContentRange(unit: String, range: Option[(Long, Long)], size: Option[Long]) {
  override def toString: String = {
    val sb = new java.lang.StringBuilder()
    sb.append(unit).append(' ')
    range match {
      case x: Some[(Long, Long)] => sb.append(x.value._1).append('-').append(x.value._2)
      case _                     => sb.append('*')
    }
    sb.append('/')
    size match {
      case x: Some[Long] => sb.append(x.value)
      case _             => sb.append('*')
    }
    sb.toString
  }
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
      case Array("*")  => Right(None)
      case Array(s, e) =>
        for {
          start <- ParseUtils.toLongOption(s).toRight(s"Invalid start of range: $s")
          end <- ParseUtils.toLongOption(e).toRight(s"Invalid end of range: $e")
        } yield Some((start, end))
      case _ => Left(s"Invalid range: $possibleRange")
    }
    val size =
      if (possibleSize.equals("*")) Right(None)
      else ParseUtils.toLongOption(possibleSize).map(Some(_)).toRight("Invalid size: %s".format(possibleSize))

    for {
      r <- range
      s <- size
      contentRange = ContentRange(unit, r, s)
      cr <- (if (isValid(contentRange)) Right(contentRange) else Left("Invalid Content-Range"))
    } yield cr
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
