package sttp.model.headers

import sttp.model.internal.Validate.RichEither

case class AcceptEncoding(compressionAlgorithm: String, weight: Option[BigDecimal]) {

  override def toString: String = s"$compressionAlgorithm${weight.map(w => s";q=$w").getOrElse("")}"
}

object AcceptEncoding {

  "Accept-Encoding: deflate, gzip;q=1.0, *;q=0.5"
  def parse(str: String): List[Either[String, AcceptEncoding]] =
    str.trim
      .split(",")
      .toList
      .map(processString)

  private def processString(s: String): Either[String, AcceptEncoding] = {
    s.split(";") match {
      case Array(algorithm, weight) =>
        val encoding = AcceptEncoding(algorithm, Some(BigDecimal(weight)))
        if (isValid(encoding)) Right(encoding)
        else Left("Invalid Encoding")
      case Array(algorithm) =>
        val encoding = AcceptEncoding(algorithm, None)
        if (isValid(encoding)) Right(encoding)
        else Left("Invalid Encoding")
      case _ => Left("Expected accept-encoding in the format: \"deflate or gzip;q=1.0\", but got: %s".format(s))
    }
  }

  private def isValid(acceptEncoding: AcceptEncoding): Boolean = {
    acceptEncoding.weight match {
      case None        => !acceptEncoding.compressionAlgorithm.isBlank
      case Some(value) => (value <= 1 && value >= 0) && !acceptEncoding.compressionAlgorithm.isBlank
    }
  }

  def unsafeParse(s: String): List[AcceptEncoding] = parse(s).map(_.getOrThrow)

  def unsafeApply(compressionAlgorithm: String, weight: Option[BigDecimal]): AcceptEncoding =
    safeApply(compressionAlgorithm, weight).getOrThrow

  def safeApply(compressionAlgorithm: String, weight: Option[BigDecimal]): Either[String, AcceptEncoding] = {
    val encoding = AcceptEncoding(compressionAlgorithm, weight)
    if (isValid(encoding)) Right(encoding)
    else Left("Invalid Content Range")
  }
}
