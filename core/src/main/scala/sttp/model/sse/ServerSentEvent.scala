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

  /** True if the event carries no data, event type, id or retry - only comments, if any. Clients ignore comments, so
    * such events (e.g. keep-alive pings) can usually be skipped.
    */
  def isCommentOnly: Boolean = data.isEmpty && eventType.isEmpty && id.isEmpty && retry.isEmpty

  override def toString: String = {
    val _comments = comments.flatMap(_.split("\n")).map(comment => Some(s": $comment")).toArray
    val _data = data.map(_.split("\n")).map(_.map(line => Some(s"data: $line"))).getOrElse(Array.empty[Option[String]])
    val _event = eventType.map(event => s"event: $event")
    val _id = id.map(id => s"id: $id")
    val _retry = retry.map(retryCount => s"retry: $retryCount")
    ((_comments ++ _data) :+ _event :+ _id :+ _retry).flatten.mkString("\n")
  }
}

object ServerSentEvent {
  // required for binary compatibility
  def apply(
      data: Option[String],
      eventType: Option[String],
      id: Option[String],
      retry: Option[Int]
  ): ServerSentEvent = new ServerSentEvent(data, eventType, id, retry, Nil)

  /** An event consisting of a single comment line. Such events are ignored by clients, and can be used to keep the
    * connection alive, so that it isn't dropped by proxies.
    */
  def comment(comment: String): ServerSentEvent = ServerSentEvent(comments = List(comment))

  // https://html.spec.whatwg.org/multipage/server-sent-events.html
  def parse(event: List[String]): ServerSentEvent = {
    event.foldLeft(ServerSentEvent()) { (event, line) =>
      if (line.startsWith(":")) event.copy(comments = event.comments :+ removeLeadingSpace(line.substring(1)))
      else if (line.startsWith("data:")) combineData(event, removeLeadingSpace(line.substring(5)))
      else if (line.startsWith("id:")) event.copy(id = Some(removeLeadingSpace(line.substring(3))))
      else if (line.startsWith("retry:"))
        event.copy(retry = ParseUtils.toIntOption(removeLeadingSpace(line.substring(6))))
      else if (line.startsWith("event:")) event.copy(eventType = Some(removeLeadingSpace(line.substring(6))))
      else if (line == "data") combineData(event, "")
      else if (line == "id") event.copy(id = Some(""))
      else if (line == "event") event.copy(eventType = Some(""))
      else event
    }
  }

  private def combineData(event: ServerSentEvent, newData: String): ServerSentEvent =
    event.copy(data = Some(event.data.fold(newData)(oldData => s"$oldData\n$newData")))

  private def removeLeadingSpace(s: String): String = if (s.startsWith(" ")) s.substring(1) else s
}
