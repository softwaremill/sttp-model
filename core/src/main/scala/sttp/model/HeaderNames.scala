package sttp.model

// https://www.iana.org/assignments/message-headers/message-headers.xml#perm-headers
trait HeaderNames {
  val Accept = "Accept"
  val AcceptCharset = "Accept-Charset"
  val AcceptEncoding = "Accept-Encoding"
  val AcceptLanguage = "Accept-Language"
  val AcceptRanges = "Accept-Ranges"
  val AccessControlAllowCredentials = "Access-Control-Allow-Credentials"
  val AccessControlAllowHeaders = "Access-Control-Allow-Headers"
  val AccessControlAllowMethods = "Access-Control-Allow-Methods"
  val AccessControlAllowOrigin = "Access-Control-Allow-Origin"
  val AccessControlExposeHeaders = "Access-Control-Expose-Headers"
  val AccessControlMaxAge = "Access-Control-Max-Age"
  val AccessControlRequestHeaders = "Access-Control-Request-Headers"
  val AccessControlRequestMethod = "Access-Control-Request-Method"
  val Age = "Age"
  val Allow = "Allow"
  val Authorization = "Authorization"
  val CacheControl = "Cache-Control"
  val Connection = "Connection"
  val ContentDisposition = "Content-Disposition"
  val ContentEncoding = "Content-Encoding"
  val ContentLanguage = "Content-Language"
  val ContentLength = "Content-Length"
  val ContentLocation = "Content-Location"
  val ContentMd5 = "Content-MD5"
  val ContentRange = "Content-Range"
  val ContentTransferEncoding = "Content-Transfer-Encoding"
  val ContentType = "Content-Type"
  val Cookie = "Cookie"
  val Date = "Date"
  val Etag = "ETag"
  val Expect = "Expect"
  val Expires = "Expires"
  val Forwarded = "Forwarded"
  val From = "From"
  val Host = "Host"
  val IfMatch = "If-Match"
  val IfModifiedSince = "If-Modified-Since"
  val IfNoneMatch = "If-None-Match"
  val IfRange = "If-Range"
  val IfUnmodifiedSince = "If-Unmodified-Since"
  val LastModified = "Last-Modified"
  val Link = "Link"
  val Location = "Location"
  val MaxForwards = "Max-Forwards"
  val Origin = "Origin"
  val Pragma = "Pragma"
  val ProxyAuthenticate = "Proxy-Authenticate"
  val ProxyAuthorization = "Proxy-Authorization"
  val Range = "Range"
  val Referer = "Referer"
  val RemoteAddress = "Remote-Address"
  val RetryAfter = "Retry-After"
  val SecWebSocketKey = "Sec-WebSocket-Key"
  val SecWebSocketExtensions = "Sec-WebSocket-Extensions"
  val SecWebSocketAccept = "Sec-WebSocket-Accept"
  val SecWebSocketProtocol = "Sec-WebSocket-Protocol"
  val SecWebSocketVersion = "Sec-WebSocket-Version"
  val Server = "Server"
  val SetCookie = "Set-Cookie"
  val StrictTransportSecurity = "Strict-Transport-Security"
  val Te = "Te"
  val Trailer = "Trailer"
  val TransferEncoding = "Transfer-Encoding"
  val Upgrade = "Upgrade"
  val UserAgent = "User-Agent"
  val Vary = "Vary"
  val Via = "Via"
  val Warning = "Warning"
  val WwwAuthenticate = "WWW-Authenticate"
  val XFrameOptions = "X-Frame-Options"
  val XForwardedFor = "X-Forwarded-For"
  val XForwardedHost = "X-Forwarded-Host"
  val XForwardedPort = "X-Forwarded-Port"
  val XForwardedProto = "X-Forwarded-Proto"
  val XRealIp = "X-Real-Ip"
  val XRequestedWith = "X-Requested-With"
  val XXSSProtection = "X-XSS-Protection"

  val ContentHeaders: Set[String] =
    Set(HeaderNames.ContentLength, HeaderNames.ContentType, HeaderNames.ContentMd5).map(_.toLowerCase())
  val SensitiveHeaders: Set[String] =
    Set(
      HeaderNames.Authorization,
      HeaderNames.ProxyAuthorization,
      HeaderNames.Cookie,
      HeaderNames.SetCookie
    ).map(_.toLowerCase())

  /** Performs a case-insensitive check, whether this header name is content-related.
    */
  def isContent(headerName: String): Boolean = ContentHeaders.contains(headerName.toLowerCase.trim)

  /** Performs a case-insensitive check, whether this header is content-related.
    */
  def isContent(header: Header): Boolean = isContent(header.name)

  /** Performs a case-insensitive check, whether this header name is sensitive.
    */
  def isSensitive(headerName: String): Boolean = isSensitive(headerName, SensitiveHeaders)

  /** Performs a case-insensitive check, whether this header name is sensitive.
    */
  def isSensitive(headerName: String, sensitiveHeaders: Set[String]): Boolean =
    sensitiveHeaders.map(_.toLowerCase()).contains(headerName.toLowerCase.trim)

  /** Performs a case-insensitive check, whether this header is sensitive.
    */
  def isSensitive(header: Header): Boolean =
    isSensitive(header.name, SensitiveHeaders)

  /** Performs a case-insensitive check, whether this header is sensitive.
    */
  def isSensitive(header: Header, sensitiveHeaders: Set[String]): Boolean =
    isSensitive(header.name, sensitiveHeaders)
}

object HeaderNames extends HeaderNames
