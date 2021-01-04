package sttp.model

import scala.collection.immutable.Seq

trait RequestMetadata extends HasHeaders {
  def method: Method
  def uri: Uri

  override def toString: String = s"RequestMetadata($method,$uri,${Headers.toStringSafe(headers)})"
}

object RequestMetadata {
  def apply(_method: Method, _uri: Uri, _headers: Seq[Header]): RequestMetadata =
    new RequestMetadata {
      override def method: Method = _method
      override def uri: Uri = _uri
      override def headers: Seq[Header] = _headers
    }
}
