package sttp.model.headers

import sttp.model.internal.ParseUtils

object Host {

  /** Parses the given string into a host and an optional port. If the host is an IPv6 address, the surrounding `[` and
    * `]` characters are removed. If the port is not a number, it is discarded.
    *
    * For example, `example.com:8080` will be parsed into `("example.com", Some(8080))`.
    */
  def parseHostAndPort(v: String): (String, Option[Int]) = {
    val lastColonIndex = v.lastIndexOf(":")
    val lastBracketIndex = v.lastIndexOf("]") // checking if the last colon is part of an IPv6 address
    val (host, port) =
      if (lastColonIndex == -1 || lastBracketIndex > lastColonIndex) (v, None)
      else {
        (v.substring(0, lastColonIndex), ParseUtils.toIntOption(v.substring(lastColonIndex + 1)))
      }

    val hostWithoutBrackets = if (host.startsWith("[") && host.endsWith("]")) {
      host.substring(1, host.length - 1)
    } else {
      host
    }

    (hostWithoutBrackets, port)
  }
}
