package sttp.model.internal

//https://www.rfc-editor.org/rfc/rfc9110.html#name-field-values
private[model] object Rfc9110 {
  private val availableWhitespaces = "\\x09\\x20"
  private val VCHAR = "\\x21-\\x7E"
  private val regex = s"^(?:^[$VCHAR]+([$availableWhitespaces]+[$VCHAR]+)*)?$$"

  def validateFieldValue(v: String): Option[String] = {
    if (v.matches(regex)) None
    else
      Some(
        """Invalid header value. The header value cannot have leading or trailing whitespace
          |and must consist of visible US-ASCII characters, including space and horizontal tab.""".stripMargin
          .replaceAll("\n", " ")
      )
  }
}
