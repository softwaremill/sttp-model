package sttp.model

trait Encodings {
  val Gzip = "gzip"
  val Compress = "compress"
  val Deflate = "deflate"
  val Br = "br"
  val Identity = "identity"
  val Wildcard = "*"
}

object Encodings extends Encodings
