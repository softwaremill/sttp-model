package sttp.model.sse

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ServerSentEventTest extends AnyFlatSpec with Matchers {
  val data = List(
    (List(": this is a test stream"), ServerSentEvent(comments = List("this is a test stream"))),
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
    ),
    (
      List(": first", "data: x", ": second"),
      ServerSentEvent(Some("x"), comments = List("first", "second"))
    ),
    (List(":no leading space"), ServerSentEvent(comments = List("no leading space"))),
    (List(":"), ServerSentEvent(comments = List(""))),
    (List("foo: bar", "data: x"), ServerSentEvent(Some("x"))),
    (List("data"), ServerSentEvent(Some(""))),
    (List("event"), ServerSentEvent(eventType = Some("")))
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

  "composeSSE" should "serialise a comment-only event" in {
    ServerSentEvent(comments = List("ping")).toString shouldBe ": ping"
  }

  "composeSSE" should "serialise comments before the other fields" in {
    val sse = ServerSentEvent(Some("d"), comments = List("c1", "c2"))
    sse.toString shouldBe
      s""": c1
         |: c2
         |data: d""".stripMargin
  }

  "parse" should "round-trip an event with comments and all other fields set" in {
    val sse = ServerSentEvent(Some("line1\nline2"), Some("evt"), Some("id1"), Some(5), List("c1", "c2"))
    ServerSentEvent.parse(sse.toString.split("\n").toList) shouldBe sse
  }

  "comment" should "create an event carrying a single comment" in {
    ServerSentEvent.comment("ping") shouldBe ServerSentEvent(comments = List("ping"))
  }

  "copy" should "preserve comments when another field is changed" in {
    ServerSentEvent(comments = List("ping")).copy(data = Some("d")) shouldBe
      ServerSentEvent(Some("d"), comments = List("ping"))
  }

  "composeSSE" should "serialise a multi-line comment as multiple comment lines" in {
    ServerSentEvent.comment("a\nb").toString shouldBe
      s""": a
         |: b""".stripMargin
  }

  "composeSSE" should "not emit a blank line for a comment ending with newlines" in {
    ServerSentEvent.comment("ping\n\n").toString shouldBe ": ping"
  }

  "composeSSE" should "serialise a comment containing a carriage return as multiple comment lines" in {
    ServerSentEvent.comment("x\rdata: y").toString shouldBe
      s""": x
         |: data: y""".stripMargin
  }

  "composeSSE" should "serialise a comment containing CRLF as multiple comment lines" in {
    ServerSentEvent.comment("a\r\nb").toString shouldBe
      s""": a
         |: b""".stripMargin
  }

  "comment" should "not allow a carriage return to inject other fields" in {
    val sse = ServerSentEvent.comment("x\rdata: y")
    ServerSentEvent.parse(sse.toString.split("\r\n|\r|\n").toList) shouldBe
      ServerSentEvent(comments = List("x", "data: y"))
  }

  "comment" should "not allow a newline to inject other fields" in {
    val sse = ServerSentEvent.comment("x\ndata: y")
    ServerSentEvent.parse(sse.toString.split("\n").toList) shouldBe
      ServerSentEvent(comments = List("x", "data: y"))
  }

  "apply" should "keep comments as they were given" in {
    ServerSentEvent(comments = List("a\nb")).comments shouldBe List("a\nb")
  }

  "comment" should "split a multi-line comment into separate comments" in {
    ServerSentEvent.comment("a\nb\rc\r\nd").comments shouldBe List("a", "b", "c", "d")
  }

  "copy" should "keep comments as they were given" in {
    ServerSentEvent().copy(comments = List("a\nb")).comments shouldBe List("a\nb")
  }

  "parse" should "round-trip an event built from a multi-line comment" in {
    val sse = ServerSentEvent.comment("a\nb")
    ServerSentEvent.parse(sse.toString.split("\r\n|\r|\n").toList) shouldBe sse
  }

  val roundTripComments = List(
    List("ping"),
    List(""),
    List("\n"),
    List("\r"),
    List("\r\n"),
    List("ping\n\n"),
    List("a\nb"),
    List("a\r\nb"),
    List("x\rdata: y"),
    List("a\n\nb"),
    List(" spaced"),
    List("a", "b"),
    List("", "b"),
    List("", "")
  )

  for (comments <- roundTripComments) {
    it should s"round-trip comments ${comments.map(_.replace("\r", "\\r").replace("\n", "\\n"))}" in {
      val sse = ServerSentEvent(Some("d1\nd2"), Some("evt"), Some("id1"), Some(7), comments)
      val serialised = sse.toString
      ServerSentEvent.parse(serialised.split("\r\n|\r|\n").toList).toString shouldBe serialised
    }
  }

  "isCommentOnly" should "be true for a keep-alive event" in {
    ServerSentEvent.comment("ping").isCommentOnly shouldBe true
  }

  "isCommentOnly" should "be true for an event with no fields set at all" in {
    ServerSentEvent().isCommentOnly shouldBe true
  }

  "isCommentOnly" should "be false when data is set" in {
    ServerSentEvent(Some("d"), comments = List("ping")).isCommentOnly shouldBe false
  }

  "isCommentOnly" should "be false when only retry is set" in {
    ServerSentEvent(retry = Some(5)).isCommentOnly shouldBe false
  }
}
