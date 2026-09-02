package sttp.model.sse

import sttp.model.internal.ParseUtils

case class ServerSentEvent(
    data: Option[String] = None,
    eventType: Option[String] = None,
    id: Option[String] = None,
    retry: Option[Int] = None
) {
  override def toString: String = {
    val _data = data
      .map(ServerSentEvent.splitOnLineTerminators)
      .map(_.map(line => Some(s"data: $line")))
      .getOrElse(Array.empty[Option[String]])
    val _event = eventType.map(event => s"event: ${ServerSentEvent.removeLineTerminators(event)}")
    val _id = id.map(id => s"id: ${ServerSentEvent.removeLineTerminators(id)}")
    val _retry = retry.map(retryCount => s"retry: $retryCount")
    (_data :+ _event :+ _id :+ _retry).flatten.mkString("\n")
  }
}

object ServerSentEvent {
  private val LineTerminators = "\r\n|\r|\n"

  // performance: split("\n") skips the regex engine; with no CR, LF is the only terminator, so it's equivalent
  private def splitOnLineTerminators(s: String): Array[String] =
    if (s.indexOf('\r') < 0) s.split("\n", -1) else s.split(LineTerminators, -1)

  private def removeLineTerminators(s: String): String =
    if (s.indexOf('\r') < 0 && s.indexOf('\n') < 0) s else s.replaceAll(LineTerminators, "")

  // https://html.spec.whatwg.org/multipage/server-sent-events.html
  def parse(event: List[String]): ServerSentEvent = {
    event.foldLeft(ServerSentEvent()) { (event, line) =>
      if (line.startsWith("data:")) combineData(event, removeLeadingSpace(line.substring(5)))
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

  private def combineData(event: ServerSentEvent, newData: String): ServerSentEvent = {
    event match {
      case e @ ServerSentEvent(Some(oldData), _, _, _) => e.copy(data = Some(s"$oldData\n$newData"))
      case e @ ServerSentEvent(None, _, _, _)          => e.copy(data = Some(newData))
    }
  }

  private def removeLeadingSpace(s: String): String = if (s.startsWith(" ")) s.substring(1) else s
}
