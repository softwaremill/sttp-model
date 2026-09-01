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
    ),
    (
      List("data: event1 data", "event: event1", "id: id1", "retry: 5"),
      ServerSentEvent(Some("event1 data"), Some("event1"), Some("id1"), Some(5))
    )
  )

  for ((lines, expected) <- data) {
    it should s"parse ${lines.size} lines starting with ${lines.headOption.getOrElse("-")}" in {
      ServerSentEvent.parse(lines) shouldBe expected
    }
  }

  "composeSSE" should "successfully serialise Server Sent Event with all fields set" in {
    val sse = ServerSentEvent(Some("data"), Some("event"), Some("id1"), Some(10))
    sse.toString shouldBe
      s"""data: data
         |event: event
         |id: id1
         |retry: 10""".stripMargin
  }

  "composeSSE" should "omit fields that are not set" in {
    val sse = ServerSentEvent(Some("data"), None, Some("id1"), None)
    sse.toString shouldBe
      s"""data: data
         |id: id1""".stripMargin
  }

  "composeSSE" should "successfully serialise multiline data event" in {
    val sse = ServerSentEvent(
      Some("""some data info 1
          |some data info 2
          |some data info 3""".stripMargin),
      None,
      None,
      None
    )

    sse.toString shouldBe
      s"""data: some data info 1
         |data: some data info 2
         |data: some data info 3""".stripMargin
  }

  "composeSSE" should "split data on all line terminators" in {
    val sse = ServerSentEvent(Some("line 1\r\nline 2\rline 3\nline 4"))

    sse.toString shouldBe
      s"""data: line 1
         |data: line 2
         |data: line 3
         |data: line 4""".stripMargin
  }

  "composeSSE" should "remove line terminators from the event type" in {
    val sse = ServerSentEvent(eventType = Some("a\ndata: injected\rb\r\nc"))
    sse.toString shouldBe "event: adata: injectedbc"
  }

  "composeSSE" should "remove line terminators from the id" in {
    val sse = ServerSentEvent(id = Some("a\ndata: injected\rb\r\nc"))
    sse.toString shouldBe "id: adata: injectedbc"
  }

  "composeSSE" should "not allow injecting fields through data, the event type or the id" in {
    val malicious = "x\r\nevent: injected\rid: injected\ndata: injected"
    val sse = ServerSentEvent(Some(malicious), Some(malicious), Some(malicious), Some(10))

    ServerSentEvent.parse(sse.toString.split("\n").toList) shouldBe ServerSentEvent(
      Some("x\nevent: injected\nid: injected\ndata: injected"),
      Some("xevent: injectedid: injecteddata: injected"),
      Some("xevent: injectedid: injecteddata: injected"),
      Some(10)
    )
  }
}
