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
    if (encodings.isEmpty) Left(s"No encodings found in: $str")
    else {
      @tailrec
      def go(es: List[WeightedEncoding], validated: List[WeightedEncoding]): Either[String, AcceptEncoding] = es match {
        case Nil => Right(AcceptEncoding(validated))
        case head :: tail =>
          validate(head, str) match {
            case Left(s)  => Left(s)
            case Right(e) => go(tail, e :: validated)
          }
      }

      go(encodings, Nil)
    }
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

  private def validate(acceptEncoding: WeightedEncoding, original: => String): Either[String, WeightedEncoding] = {
    if (acceptEncoding.encoding.isEmpty) {
      Left(s"Invalid empty encoding in: $original")
    } else {
      acceptEncoding.weight match {
        case Some(value) if value < 0 || value > 1 =>
          Left(s"Invalid weight, expected a number between 0 and 1, but got: $value in $original.")
        case _ => Right(acceptEncoding)
      }
    }
  }

  def unsafeParse(s: String): AcceptEncoding = parse(s).getOrThrow

  def unsafeApply(encoding: String, weight: Option[BigDecimal]): AcceptEncoding =
    safeApply(encoding, weight).getOrThrow

  def safeApply(encoding: String, weight: Option[BigDecimal]): Either[String, AcceptEncoding] = {
    val encodingObject = WeightedEncoding(encoding, weight)
    validate(encodingObject, encodingObject.toString).right.map(e => AcceptEncoding(List(e)))
  }
}
