package sttp.model

import scala.collection.immutable.Seq

trait ResponseMetadata extends HasHeaders {
  def code: StatusCode
  def statusText: String
  def is200: Boolean = code == StatusCode.Ok
  def isSuccess: Boolean = code.isSuccess
  def isRedirect: Boolean = code.isRedirect
  def isClientError: Boolean = code.isClientError
  def isServerError: Boolean = code.isServerError

  override def toString: String = s"ResponseMetadata($code,$statusText,${Headers.toStringSafe(headers)})"
}

object ResponseMetadata {
  def apply(statusCode: StatusCode, _statusText: String, _headers: Seq[Header]): ResponseMetadata =
    new ResponseMetadata {
      override def code: StatusCode = statusCode
      override def statusText: String = _statusText
      override def headers: Seq[Header] = _headers
    }
}
