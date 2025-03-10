package sttp.model.headers

import sttp.model.internal.Validate

/** @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Forwarded */
case class Forwarded(by: Option[String], `for`: Option[String], host: Option[String], proto: Option[String]) {

  /** Serialize a single [[Forwarded]] header to a string
    *
    * @see
    *   [[Forwarded#toString]] for a multi-header variant
    */
  override def toString: String = {
    val sb = new java.lang.StringBuilder()
    var separator = ""
    by.foreach { v => sb.append("by=").append(v); separator = ";" }
    `for`.foreach { v => sb.append(separator).append("for=").append(v); separator = ";" }
    host.foreach { v => sb.append(separator).append("host=").append(v); separator = ";" }
    proto.foreach { v => sb.append(separator).append("proto=").append(v) }
    sb.toString
  }
}

object Forwarded {

  /** Parses a list of Forwarded headers. Each header can contain multiple Forwarded values. */
  def parse(headerValues: List[String]): Either[String, List[Forwarded]] = {
    Validate.sequence(headerValues.map(parse)).map(_.flatten)
  }

  /** Parses a single Forwarded header, which can contain multiple Forwarded values. */
  def parse(headerValue: String): Either[String, List[Forwarded]] = {
    def parseSingle(headerValue: String): Either[String, Forwarded] = {
      val parts = headerValue.split(";").map(_.trim).toList
      val kvPairs = parts.map { part =>
        part.split("=").map(_.trim).toList match {
          case key :: value :: Nil => Right(key.toLowerCase -> value)
          case _                   => Left(s"Invalid part: $part")
        }
      }

      val pairs = kvPairs.collect { case Right(pair) => pair }
      if (pairs.size == kvPairs.size) {
        val by = pairs.collectFirst { case ("by", v) => v }
        val `for` = pairs.collectFirst { case ("for", v) => v }
        val host = pairs.collectFirst { case ("host", v) => v }
        val proto = pairs.collectFirst { case ("proto", v) => v }
        Right(Forwarded(by, `for`, host, proto))
      } else
        Left(kvPairs.collect { case Left(error) => error }.mkString(", "))
    }

    Validate.sequence(headerValue.split(",").map(_.trim).toList.map(parseSingle))
  }

  /** Serialize a list of [[Forwarded]] headers to a single string. Each header will be separated by a comma. */
  def toString(headers: List[Forwarded]): String = headers.map(_.toString).mkString(",")
}
