package sttp.model

import scala.collection.immutable.Seq

trait RequestMetadata extends HasHeaders {
  def method: Method
  def uri: Uri
  def tags: Map[String, Any]

  override def toString: String = s"RequestMetadata($method,$uri,${Headers.toStringSafe(headers)},$tags)"
}

object RequestMetadata {
  def apply(_method: Method, _uri: Uri, _headers: Seq[Header], _tags: Map[String, Any] = Map.empty): RequestMetadata =
    new RequestMetadata {
      override def method: Method = _method
      override def uri: Uri = _uri
      override def tags: Map[String, Any] = _tags
      override def headers: Seq[Header] = _headers
    }
}
