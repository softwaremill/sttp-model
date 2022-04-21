package sttp.model

/** Represents query parameters, where each parameter can have 0, 1, or more values. All query parameters are assumed to
  * be decoded.
  */
case class QueryParams(ps: Seq[(String, Seq[String])]) {
  def toMap: Map[String, String] = toSeq.toMap
  def toMultiMap: Map[String, Seq[String]] = ps.toMap
  def toSeq: Seq[(String, String)] = ps.flatMap { case (k, vs) => vs.map((k, _)) }
  def toMultiSeq: Seq[(String, Seq[String])] = ps

  def get(s: String): Option[String] = ps.find(_._1 == s).flatMap(_._2.headOption)
  def getMulti(s: String): Option[Seq[String]] = ps.find(_._1 == s).map(_._2)

  def param(k: String, v: String): QueryParams = new QueryParams(ps :+ (k -> List(v)))
  def param(k: String, v: Seq[String]): QueryParams = new QueryParams(ps :+ (k -> v))
  def param(p: Map[String, String]): QueryParams = new QueryParams(ps ++ p.map { case (k, v) => (k -> List(v)) })

  override def toString: String = toString(includeBoundary = false)
  def toString(includeBoundary: Boolean): String =
    toString(Uri.QuerySegmentEncoding.Standard, Uri.QuerySegmentEncoding.StandardValue, includeBoundary)
  def toString(keyEncoding: Uri.Encoding, valueEncoding: Uri.Encoding): String =
    toString(keyEncoding, valueEncoding, includeBoundary = false)

  /** @param includeBoundary
    *   Should the boundary `?` character be added preceding the result, if the query params are non-empty.
    */
  def toString(keyEncoding: Uri.Encoding, valueEncoding: Uri.Encoding, includeBoundary: Boolean): String = {
    val boundary = if (includeBoundary && ps.nonEmpty) "?" else ""
    boundary + ps
      .flatMap {
        case (k, vs) if vs.isEmpty => List(keyEncoding(k))
        case (k, vs)               => vs.map(v => s"${keyEncoding(k)}=${valueEncoding(v)}")
      }
      .mkString("&")
  }
}

object QueryParams {
  def apply() = new QueryParams(Nil)
  def fromMap(m: Map[String, String]): QueryParams = new QueryParams(m.mapValues(List(_)).toSeq)
  def fromSeq(s: Seq[(String, String)]): QueryParams = {
    // Building the sequence of parameters so that their relative order is kept. O(n^2), but there shouldn't be too many query parameters.
    def add(acc: List[(String, List[String])], kv: (String, String)): List[(String, List[String])] = {
      val (k, v) = kv
      def go(current: List[(String, List[String])]): List[(String, List[String])] = current match {
        case Nil                          => (k -> (v :: Nil)) :: Nil
        case head :: tail if head._1 == k => (head._1, head._2 :+ v) :: tail
        case head :: tail                 => head :: go(tail)
      }

      go(acc)
    }

    new QueryParams(s.foldLeft(Nil: List[(String, List[String])])(add))
  }

  def fromMultiMap(m: Map[String, Seq[String]]): QueryParams = new QueryParams(m.toSeq)
  def fromMultiSeq(m: Seq[(String, Seq[String])]): QueryParams = new QueryParams(m)
}
