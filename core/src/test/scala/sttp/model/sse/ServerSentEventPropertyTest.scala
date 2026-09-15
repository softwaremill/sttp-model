package sttp.model.sse

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ServerSentEventPropertyTest extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  private val LineTerminators = "\r\n|\r|\n"

  private val fieldValue: Gen[String] = Gen
    .listOf(
      Gen.frequency(
        6 -> Gen.oneOf("a", "b", ":", " "),
        2 -> Gen.oneOf("data: ", "event: ", "id: ", "retry: 9", "data", "id", "event", ": "),
        1 -> Gen.oneOf("\n", "\r", "\r\n")
      )
    )
    .map(_.mkString)

  private val terminatorFreeValue: Gen[String] =
    Gen.listOf(Gen.oneOf("a", "b", ":", " ", "data: ", "event: ", "id: ", "data")).map(_.mkString)

  private def eventsOf(value: Gen[String]): Gen[ServerSentEvent] = for {
    data <- Gen.option(value)
    eventType <- Gen.option(value)
    id <- Gen.option(value)
    // a negative retry serialises, but the spec allows only digits, so parsing drops it - it can't round-trip
    retry <- Gen.option(Gen.chooseNum(0, Int.MaxValue))
    comments <- Gen.listOf(value)
  } yield ServerSentEvent(data, eventType, id, retry, comments)

  private val events = eventsOf(fieldValue)
  private val terminatorFreeEvents = eventsOf(terminatorFreeValue)

  private val allowedPrefixes = List("data:", "event:", "id:", "retry:", ":")

  private def lines(serialised: String): List[String] = serialised.split(LineTerminators, -1).toList

  private def lineCount(s: String): Int = s.split(LineTerminators, -1).length

  it should "serialise every line as a comment or a known field" in {
    forAll(events) { sse =>
      val serialised = sse.toString
      if (serialised.nonEmpty) {
        lines(serialised).foreach { line =>
          withClue(s"line [$line] of [$serialised]: ") {
            allowedPrefixes.exists(line.startsWith) shouldBe true
          }
        }
      }
    }
  }

  it should "serialise one line per comment line, per data line and per other field that is set" in {
    forAll(events) { sse =>
      val expected = sse.comments.map(lineCount).sum + sse.data.fold(0)(lineCount) +
        List(sse.eventType, sse.id, sse.retry).count(_.isDefined)
      lines(sse.toString).size shouldBe math.max(expected, 1)
    }
  }

  it should "serialise, parse and serialise again to the same result" in {
    forAll(events) { sse =>
      val serialised = sse.toString
      ServerSentEvent.parse(lines(serialised)).toString shouldBe serialised
    }
  }

  it should "parse back exactly what was serialised, when no value contains a line terminator" in {
    forAll(terminatorFreeEvents) { sse =>
      ServerSentEvent.parse(lines(sse.toString)) shouldBe sse
    }
  }

  it should "parse any lines without throwing" in {
    val anyLine = Gen.oneOf(
      fieldValue,
      Gen.oneOf(
        "",
        ":",
        "data:",
        "data",
        "id:",
        "id",
        "event:",
        "event",
        "retry:",
        "retry",
        "retry: x",
        "retry: 99999999999999999999",
        "foo: bar"
      )
    )
    forAll(Gen.listOf(anyLine)) { ls => noException should be thrownBy ServerSentEvent.parse(ls) }
  }
}
