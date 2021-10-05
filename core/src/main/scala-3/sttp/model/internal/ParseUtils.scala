package sttp.model.internal

object ParseUtils {

  def toLongOption(s: String): Option[Long] = s.toLongOption

  def toIntOption(s: String): Option[Int] = s.toIntOption
}
