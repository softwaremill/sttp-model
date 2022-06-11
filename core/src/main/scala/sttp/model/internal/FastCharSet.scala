package sttp.model.internal

// Immutable set with characters. Should only be used for
// "small" characters (characters with small charcode).
// Main advantage of this class over plain scala Set is that
// it has better performance. Probably because it does less
// auto-boxing.
private[model] case class FastCharSet(set: Set[Char]) {

  private val (maxCode, array) = {
    val codes = set.map(_.toInt)
    val maxCode = codes.max
    val array = new Array[Boolean](maxCode + 1)
    codes.foreach { code =>
      array.update(code, true)
    }
    (maxCode, array)
  }

  def contains(ch: Char): Boolean = {
    val code = ch.toInt
    if (code <= maxCode) {
      array(code)
    } else {
      false
    }
  }
}

private[model] case class FastCharMap[V](map: Map[Char, V]) {

  private val (maxCode, array) = {
    val codes = map.map { case (key, value) => (key.toInt, value) }
    val maxCode = codes.maxBy(_._1)._1
    val array = Array.fill[Option[V]](maxCode + 1)(None)
    codes.foreach { case (code, value) =>
      array.update(code, Some(value))
    }
    (maxCode, array)
  }

  def get(ch: Char): Option[V] = {
    val code = ch.toInt
    if (code <= maxCode) {
      array(code)
    } else {
      None
    }
  }

  val keySet: FastCharSet =
    FastCharSet(map.keySet)
}
