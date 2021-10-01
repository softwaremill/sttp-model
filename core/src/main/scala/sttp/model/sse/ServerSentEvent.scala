package sttp.model.sse

import sttp.model.internal.ParseUtils

case class ServerSentEvent(
    data: Option[String] = None,
    eventType: Option[String] = None,
    id: Option[String] = None,
    retry: Option[Int] = None
) {
  override def toString: String = {
    val _data = data.map(_.split("\n")).map(_.map(line => Some(s"data: $line"))).getOrElse(Array.empty[Option[String]])
    val _event = eventType.map(event => s"event: $event")
    val _id = id.map(id => s"id: $id")
    val _retry = retry.map(retryCount => s"retry: $retryCount")
    (_data :+ _event :+ _id :+ _retry).flatten.mkString("\n")
  }
}

object ServerSentEvent {
  // https://html.spec.whatwg.org/multipage/server-sent-events.html
  def parse(event: List[String]): ServerSentEvent = {
    event.foldLeft(ServerSentEvent()) { (event, line) =>
      if (line.startsWith("data:")) combineData(event, removeLeadingSpace(line.substring(5)))
      else if (line.startsWith("id:")) event.copy(id = Some(removeLeadingSpace(line.substring(3))))
      else if (line.startsWith("retry:")) event.copy(retry = ParseUtils.toIntOption(line.substring(6)))
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
