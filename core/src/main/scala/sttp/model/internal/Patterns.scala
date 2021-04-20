package sttp.model.internal

import java.util.regex.Pattern
import scala.util.matching.Regex

private[model] object Patterns {

  /** token can consist any char except separators defined in rfc
    * https://tools.ietf.org/html/rfc2616#section-2.2
    */
  private val Token = "([a-zA-Z0-9-!#$%&'*+.^_`{|}~]+)"
  private val Quoted = "\"([^\"]*)\""

  val Type: Pattern = Pattern.compile(s"$Token")
  val TypeSubtype: Pattern = Pattern.compile(s"$Token/$Token")
  val Parameter: Pattern = Pattern.compile(s";\\s*(?:$Token=(?:$Token|$Quoted))?")

  val QValue: Regex = "(0\\.?\\d{0,3}?|\\.\\d{1,3}?|1\\.0{1,3}?)".r
  val WhiteSpaces: String = "\\s+"

  def parseMediaTypeParameters(t: String, offset: Int): Either[String, Map[String, String]] = {
    var parameters: Map[String, String] = Map.empty
    val parameter = Parameter.matcher(t)
    var s = offset
    val length = t.length

    while (s < length) {
      parameter.region(s, length)
      if (!parameter.lookingAt()) {
        return Left(s"""Parameter is not formatted correctly: "${t.substring(s)}" for: "$t"""")
      }

      val name = parameter.group(1)
      if (name == null) {
        s = parameter.end()
      } else {
        val token = parameter.group(2)
        val strippedToken = parameter.group(2) match {
          case null =>
            // Value is "double-quoted". That's valid and our regex group already strips the quotes.
            parameter.group(3)
          case _ if token.startsWith("'") && token.endsWith("'") && token.length > 2 =>
            // If the token is 'single-quoted' it's invalid! But we're lenient and strip the quotes.
            token.substring(1, token.length - 1)
          case _ => token
        }

        parameters.get(`name`) match {
          case Some(duplicate) =>
            return Left(s"""Multiple "$name" defined: "$duplicate" and: "$strippedToken" for: "$t"""")
          case _ => ()
        }

        parameters += name -> strippedToken
        s = parameter.end()
      }
    }
    Right(parameters)
  }
}
