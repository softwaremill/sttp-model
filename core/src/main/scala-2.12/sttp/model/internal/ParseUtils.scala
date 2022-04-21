package sttp.model.internal

import scala.util.Try

object ParseUtils {

  def toLongOption(s: String): Option[Long] = Try(s.toLong).toOption

  def toIntOption(s: String): Option[Int] = Try(s.toInt).toOption
}
