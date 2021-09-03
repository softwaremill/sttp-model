package sttp.model.internal

import java.net.URLEncoder

import sttp.model.internal.idn.IdnApi

private[sttp] object UriCompatibility {
  def encodeDNSHost(host: String): String = {
    val noSpecialChars = if (host.contains("..")) {
      // java.net.IDN.toASCII requires only non-empty labels (host segments), and throws an exception if there's one.
      // However, this is a valid host as specified by rfc3986.
      host
        .split("\\.")
        .map {
          case "" => ""
          case x  => IdnApi.toAscii(x)
        }
        .mkString(".")
    } else IdnApi.toAscii(host)
    Rfc3986.encode(Rfc3986.Host)(noSpecialChars)
  }

  def encodeQuery(s: String, enc: String): String = URLEncoder.encode(s, enc)
}
