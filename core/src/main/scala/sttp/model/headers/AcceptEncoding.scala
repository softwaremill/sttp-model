package sttp.model.headers

import sttp.model.headers.AcceptEncoding.WeightedEncoding
import sttp.model.internal.Validate.RichEither

import scala.annotation.tailrec

case class AcceptEncoding(encodings: List[WeightedEncoding]) {

  override def toString: String = encodings.map(_.toString).mkString(",")
}

object AcceptEncoding {

  case class WeightedEncoding(encoding: String, weight: Option[BigDecimal]) {
    override def toString: String = s"$encoding${weight.map(w => s";q=$w").getOrElse("")}"
  }

  def parse(str: String): Either[String, AcceptEncoding] = {
    val encodings = processString(str, List.empty)
    if (encodings.forall(isValid) && encodings.nonEmpty) Right(AcceptEncoding(encodings.reverse))
    else if (encodings.isEmpty)
      Left("Expected Accept-Encoding in the format: \"deflate\" or \"gzip;q=1.0\", but got: %s".format(str))
    else Left("%s contains one or more Encodings with empty name or incorrect weight".format(str))
  }

  @tailrec
  private def processString(str: String, acc: List[WeightedEncoding]): List[WeightedEncoding] = {
    str.trim.split(",").toList match {
      case x :: tail if x.nonEmpty =>
        val range = parsSingleEncoding(x)
        processString(tail.mkString(","), range :: acc)
      case Nil => List(parsSingleEncoding(str))
      case _   => acc
    }
  }

  private def parsSingleEncoding(s: String): WeightedEncoding = {
    s.split(";") match {
      case Array(algorithm, weight) =>
        weight.split("=") match {
          case Array(_, value) => WeightedEncoding(algorithm, Some(BigDecimal(value)))
          case _               => WeightedEncoding("", None)
        }
      case Array(algorithm) => WeightedEncoding(algorithm, None)
      case _                => WeightedEncoding("", None)
    }
  }

  private def isValid(acceptEncoding: WeightedEncoding): Boolean = {
    acceptEncoding.weight match {
      case None        => acceptEncoding.encoding.nonEmpty
      case Some(value) => (value <= 1 && value >= 0) && acceptEncoding.encoding.nonEmpty
    }
  }

  def unsafeParse(s: String): AcceptEncoding = parse(s).getOrThrow

  def unsafeApply(encoding: String, weight: Option[BigDecimal]): AcceptEncoding =
    safeApply(encoding, weight).getOrThrow

  def safeApply(encoding: String, weight: Option[BigDecimal]): Either[String, AcceptEncoding] = {
    val encodingObject = WeightedEncoding(encoding, weight)
    if (isValid(encodingObject)) Right(AcceptEncoding(List(encodingObject)))
    else
      Left(
        "Expected Encoding in the format: \"deflate\" or \"gzip;q=1.0\", but got: %s".format(encodingObject.toString)
      )
  }
}
