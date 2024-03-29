package sttp.model

import sttp.model.StatusCode._

case class StatusText(text: String)

object StatusText {

  private val statusTexts: Map[StatusCode, String] = Map(
    Continue -> "Continue",
    SwitchingProtocols -> "Switching Protocols",
    Processing -> "Processing",
    EarlyHints -> "Early Hints",
    Ok -> "Ok",
    Created -> "Created",
    Accepted -> "Accepted",
    NonAuthoritativeInformation -> "Non Authoritative Information",
    NoContent -> "No Content",
    ResetContent -> "Reset Content",
    PartialContent -> "Partial Content",
    MultiStatus -> "Multi Status",
    AlreadyReported -> "Already Reported",
    ImUsed -> "Im Used",
    MultipleChoices -> "Multiple Choices",
    MovedPermanently -> "Moved Permanently",
    Found -> "Found",
    SeeOther -> "See Other",
    NotModified -> "Not Modified",
    UseProxy -> "Use Proxy",
    TemporaryRedirect -> "Temporary Redirect",
    PermanentRedirect -> "Permanent Redirect",
    BadRequest -> "Bad Request",
    Unauthorized -> "Unauthorized",
    PaymentRequired -> "Payment Required",
    Forbidden -> "Forbidden",
    NotFound -> "Not Found",
    MethodNotAllowed -> "Method Not Allowed",
    NotAcceptable -> "Not Acceptable",
    ProxyAuthenticationRequired -> "Proxy Authentication Required",
    RequestTimeout -> "Request Timeout",
    Conflict -> "Conflict",
    Gone -> "Gone",
    LengthRequired -> "Length Required",
    PreconditionFailed -> "Precondition Failed",
    PayloadTooLarge -> "Payload Too Large",
    UriTooLong -> "Uri Too Long",
    UnsupportedMediaType -> "Unsupported MediaType",
    RangeNotSatisfiable -> "Range Not Satisfiable",
    ExpectationFailed -> "Expectation Failed",
    MisdirectedRequest -> "Misdirected Request",
    UnprocessableEntity -> "Unprocessable Entity",
    Locked -> "Locked",
    FailedDependency -> "Failed Dependency",
    UpgradeRequired -> "Upgrade Required",
    PreconditionRequired -> "Precondition Required",
    TooManyRequests -> "Too Many Requests",
    RequestHeaderFieldsTooLarge -> "RequestHeader Fields Too Large",
    UnavailableForLegalReasons -> "Unavailable For Legal Reasons",
    InternalServerError -> "Internal Server Error",
    NotImplemented -> "Not Implemented",
    BadGateway -> "Bad Gateway",
    ServiceUnavailable -> "Service Unavailable",
    GatewayTimeout -> "Gateway Timeout",
    HttpVersionNotSupported -> "Http Version Not Supported",
    VariantAlsoNegotiates -> "Variant Also Negotiates",
    InsufficientStorage -> "Insufficient Storage",
    LoopDetected -> "Loop Detected",
    NotExtended -> "Not Extended",
    NetworkAuthenticationRequired -> "Network Authentication Required"
  )

  def default(statusCode: StatusCode): Option[String] =
    statusTexts.get(statusCode)
}
