package sttp.model.sse

import sttp.model.internal.ParseUtils

case class ServerSentEvent(
    data: Option[String] = None,
    eventType: Option[String] = None,
    id: Option[String] = None,
    retry: Option[Int] = None,
    comments: List[String] = Nil
) {
  // required for binary compatibility
  def this(data: Option[String], eventType: Option[String], id: Option[String], retry: Option[Int]) =
    this(data, eventType, id, retry, Nil)

  def copy(
      data: Option[String] = this.data,
      eventType: Option[String] = this.eventType,
      id: Option[String] = this.id,
      retry: Option[Int] = this.retry,
      comments: List[String] = this.comments
  ): ServerSentEvent = ServerSentEvent(data, eventType, id, retry, comments)

  // required for binary compatibility
  def copy(
      data: Option[String],
      eventType: Option[String],
      id: Option[String],
      retry: Option[Int]
  ): ServerSentEvent = ServerSentEvent(data, eventType, id, retry, this.comments)

  /** True if the event carries no data, event type, id or retry. Clients ignore comments, so such events - keep-alive
    * pings, but also blocks made up of unknown fields - carry nothing for the application and can usually be skipped.
    */
  def hasNoFields: Boolean = data.isEmpty && eventType.isEmpty && id.isEmpty && retry.isEmpty

  override def toString: String = {
    val _comments =
      comments.flatMap(ServerSentEvent.splitOnLineTerminators).map(comment => Some(s": $comment")).toArray
    val _data = data
      .map(ServerSentEvent.splitOnLineTerminators)
      .map(_.map(line => Some(s"data: $line")))
      .getOrElse(Array.empty[Option[String]])
    val _event = eventType.map(event => s"event: ${ServerSentEvent.removeLineTerminators(event)}")
    val _id = id.map(id => s"id: ${ServerSentEvent.removeLineTerminators(id)}")
    val _retry = retry.map(retryCount => s"retry: $retryCount")
    val _fields = _data :+ _event :+ _id :+ _retry
    val _all = if (_comments.isEmpty) _fields else _comments ++ _fields
    _all.flatten.mkString("\n")
  }
}

object ServerSentEvent {
  private val LineTerminators = "\r\n|\r|\n"

  // performance: split("\n") skips the regex engine; with no CR, LF is the only terminator, so it's equivalent
  private def splitOnLineTerminators(s: String): Array[String] =
    if (s.indexOf('\r') < 0) s.split("\n", -1) else s.split(LineTerminators, -1)

  private def removeLineTerminators(s: String): String =
    if (s.indexOf('\r') < 0 && s.indexOf('\n') < 0) s else s.replaceAll(LineTerminators, "")

  // required for binary compatibility
  def apply(
      data: Option[String],
      eventType: Option[String],
      id: Option[String],
      retry: Option[Int]
  ): ServerSentEvent = new ServerSentEvent(data, eventType, id, retry, Nil)

  /** An event consisting of comment lines only, one per line of the given text. Such events are ignored by clients, and
    * can be used to keep the connection alive, so that it isn't dropped by proxies.
    */
  def comment(text: String): ServerSentEvent =
    ServerSentEvent(comments = splitOnLineTerminators(text).toList)

  // https://html.spec.whatwg.org/multipage/server-sent-events.html
  def parse(event: List[String]): ServerSentEvent = {
    // comments are prepended and reversed once at the end for performance
    val parsed = event.foldLeft(ServerSentEvent()) { (event, line) =>
      if (line.startsWith(":")) event.copy(comments = removeLeadingSpace(line.substring(1)) :: event.comments)
      else if (line.startsWith("data:")) combineData(event, removeLeadingSpace(line.substring(5)))
      else if (line.startsWith("id:")) event.copy(id = Some(removeLeadingSpace(line.substring(3))))
      else if (line.startsWith("retry:")) combineRetry(event, removeLeadingSpace(line.substring(6)))
      else if (line.startsWith("event:")) event.copy(eventType = Some(removeLeadingSpace(line.substring(6))))
      else if (line == "data") combineData(event, "")
      else if (line == "id") event.copy(id = Some(""))
      else if (line == "event") event.copy(eventType = Some(""))
      else event
    }
    if (parsed.comments.isEmpty) parsed else parsed.copy(comments = parsed.comments.reverse)
  }

  /** The spec accepts only ASCII digits here, and says to ignore the field otherwise - so a value that isn't accepted
    * leaves any previously parsed one in place. `toIntOption` is still needed to reject a value too large for an `Int`,
    * and `isDigit` would not do instead of the range check: it, like `toIntOption`, accepts non-ASCII digits.
    */
  private def combineRetry(event: ServerSentEvent, newRetry: String): ServerSentEvent =
    if (newRetry.nonEmpty && newRetry.forall(c => c >= '0' && c <= '9'))
      ParseUtils.toIntOption(newRetry).fold(event)(retry => event.copy(retry = Some(retry)))
    else event

  private def combineData(event: ServerSentEvent, newData: String): ServerSentEvent =
    event.copy(data = Some(event.data.fold(newData)(oldData => s"$oldData\n$newData")))

  private def removeLeadingSpace(s: String): String = if (s.startsWith(" ")) s.substring(1) else s
}
