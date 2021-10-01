package sttp.model.internal

import scala.util.Try

object ParseUtils {

  def toLongOption(s: String): Option[Long] =
    Try(s.toLong).toOption

}