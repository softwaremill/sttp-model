package sttp.model

import internal.Validate._
import sttp.model.internal.Validate
import sttp.model.internal.Rfc2616.validateToken

case class Method(method: String) extends AnyVal {
  override def toString: String = method
  def is(m: Method): Boolean = method.equalsIgnoreCase(m.method)
}

/** For a description of the behavior of `apply`, `safeApply` and `unsafeApply` methods, see [[sttp.model]].
  */
object Method extends Methods {

  /** @throws IllegalArgumentException If the method value is not a valid token. */
  def unsafeApply(method: String): Method = safeApply(method.toUpperCase).getOrThrow
  def safeApply(method: String): Either[String, Method] =
    Validate.all(validateToken("Method", method))(apply(method.toUpperCase))

  /** An HTTP method is idempotent if an identical request can be made once or several times in a row with the same
    * effect while leaving the server in the same state.
    * @see
    *   https://developer.mozilla.org/en-US/docs/Glossary/Idempotent
    */
  def isIdempotent(m: Method): Boolean = idempotent.contains(m)

  /** An HTTP method is safe if it doesn't alter the state of the server.
    * @see
    *   https://developer.mozilla.org/en-US/docs/Glossary/safe
    */
  def isSafe(m: Method): Boolean = safe.contains(m)

  private val idempotent: Set[Method] =
    Set(Method.HEAD, Method.TRACE, Method.GET, Method.PUT, Method.OPTIONS, Method.DELETE)
  private val safe: Set[Method] = Set(Method.HEAD, Method.GET, Method.OPTIONS)
}

trait Methods {
  val GET: Method = Method("GET")
  val HEAD: Method = Method("HEAD")
  val POST: Method = Method("POST")
  val PUT: Method = Method("PUT")
  val DELETE: Method = Method("DELETE")
  val OPTIONS: Method = Method("OPTIONS")
  val PATCH: Method = Method("PATCH")
  val CONNECT: Method = Method("CONNECT")
  val TRACE: Method = Method("TRACE")
}
