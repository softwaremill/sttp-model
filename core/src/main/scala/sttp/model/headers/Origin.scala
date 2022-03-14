package sttp.model.headers

sealed trait Origin
object Origin {
  case object Null extends Origin { override def toString: String = "null" }
  case class Host(scheme: String, hostname: String, port: Option[Int] = None) {
    override def toString: String = s"$scheme://$hostname${port.map(p => s":$p").getOrElse("")}"
  }
}
