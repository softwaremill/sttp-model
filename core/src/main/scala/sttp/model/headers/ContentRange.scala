package sttp.model.headers

import sttp.model.ContentRangeUnits

import scala.util.Try

case class ContentRange(start: Int, end: Int, unit: String) {

  val range: Range = Range(start, end)
}

object ContentRange {

  def fromString(str: String): Either[String, ContentRange] = {
    Try({
      val splited = str.split("=")
      val unit = splited(0)
      val range = splited(1).split("-")
      // TODO add support for other cases of range
      ContentRange(range(0).toInt, range(1).toInt, unit)
    }).toEither.left
      .map(_.getMessage)
  }
}

case class Range(start: Int, end: Int) {
  def isValid(contentLength: Long): Boolean = if (end < contentLength) true else false

  def contentRange(fileSize: Long, unit: ContentRangeUnits): String = unit + " " + start + "-" + end + "/" + fileSize

  val contentLength: Int = end - start
}