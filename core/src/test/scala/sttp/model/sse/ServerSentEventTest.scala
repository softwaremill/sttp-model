package sttp.model.sse

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ServerSentEventTest extends AnyFlatSpec with Matchers {
  val data = List(
    (List(": this is a test stream"), ServerSentEvent()),
    (List("data: some text"), ServerSentEvent(Some("some text"))),
    (List("data:  some text"), ServerSentEvent(Some(" some text"))),
    (List("data: another message", "data: with two lines"), ServerSentEvent(Some("another message\nwith two lines"))),
    (
      List("event: userconnect", "data: {\"username\": \"bobby\", \"time\": \"02:33:48\"}"),
      ServerSentEvent(Some("{\"username\": \"bobby\", \"time\": \"02:33:48\"}"), Some("userconnect"))
    ),
    (
      List("data:second event", "id"),
      ServerSentEvent(Some("second event"), id = Some(""))
    ),
    (
      List("data:x", "retry:50"),
      ServerSentEvent(Some("x"), retry = Some(50))
    )
  )

  for ((lines, expected) <- data) {
    it should s"parse ${lines.size} lines starting with ${lines.headOption.getOrElse("-")}" in {
      ServerSentEvent.parse(lines) shouldBe expected
    }
  }


  "composeSSE" should "successfully serialise simple Server Sent Event to ByteString" in {
    val sse = ServerSentEvent(Some("data"), Some("event"), Some("id1"), Some(10))
    ServerSentEvent.composeSSE(sse) shouldBe
      s"""data: data
         |event: event
         |id: id1
         |retry: 10""".stripMargin
  }

  "composeSSE" should "omit fields that are not set" in {
    val sse = ServerSentEvent(Some("data"), None, Some("id1"), None)
    ServerSentEvent.composeSSE(sse) shouldBe
      s"""data: data
         |id: id1""".stripMargin
  }

  "composeSSE" should "successfully serialise multiline data event" in {
    val sse = ServerSentEvent(
      Some(
        """some data info 1
          |some data info 2
          |some data info 3""".stripMargin),
      None,
      None,
      None
    )

    ServerSentEvent.composeSSE(sse) shouldBe
      s"""data: some data info 1
         |data: some data info 2
         |data: some data info 3""".stripMargin
  }
}
