package sttp.model.headers

import sttp.model.{Header, HeaderNames, MediaType}

import scala.collection.immutable.Seq

private[model] sealed trait Accept[A] {
  def parse(headers: Seq[Header]): Seq[(A, Float)]

  protected def extract(headers: Seq[Header], name: String, convert: String => A): Seq[(A, Float)] =
    headers
      .filter(_.is(name))
      .flatMap(_.value.split(",").flatMap { part =>
        part.replaceAll("\\s+", "").split(";").toList match {
          case value :: Nil => Some((convert(value), 1f))
          case value :: params =>
            val q = params collectFirst { case QPattern(q, _) => q }
            Some((convert(value), q.map(_.toFloat).getOrElse(1f)))
          case _ => None
        }
      })

  private val QPattern = "q=([0-1](.\\d\\d?\\d?)?)".r
}

private[model] object AcceptHeader extends Accept[MediaType] {
  override def parse(headers: Seq[Header]): Seq[(MediaType, Float)] =
    extract(headers, HeaderNames.Accept, MediaType.unsafeParse).sortBy {
      case (mt, q) if mt.isTypeAny    => (2, -q) // unbounded ranges comes last
      case (mt, q) if mt.isSubTypeAny => (1, -q) // bounded ranges comes next
      case (_, q)                     => (0, -q) // most specific comes first
    }
}

private[model] object AcceptCharsetHeader extends Accept[String] {
  override def parse(headers: Seq[Header]): Seq[(String, Float)] =
    extract(headers, HeaderNames.AcceptCharset, identity).sortBy {
      case ("*", _) => 1f // unbounded come last
      case (_, q)   => -q // others come first
    }
}
