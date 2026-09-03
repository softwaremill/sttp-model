package sttp.model.sse

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ServerSentEventPropertyTest extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 500)

  private val fieldValue: Gen[String] = Gen.listOf(Gen.oneOf('a', 'b', ':', ' ', '\r', '\n')).map(_.mkString)

  private val events: Gen[ServerSentEvent] = for {
    data <- Gen.option(fieldValue)
    eventType <- Gen.option(fieldValue)
    id <- Gen.option(fieldValue)
    retry <- Gen.option(Gen.chooseNum(0, 100000))
    comments <- Gen.listOf(fieldValue)
  } yield ServerSentEvent(data, eventType, id, retry, comments)

  private val allowedPrefixes = List("data:", "event:", "id:", "retry:", ":")

  private def lines(serialised: String): List[String] = serialised.split("\r\n|\r|\n", -1).toList

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

  it should "never serialise a blank line, which would end the event" in {
    forAll(events) { sse =>
      val serialised = sse.toString
      if (serialised.nonEmpty) lines(serialised) should not contain ""
    }
  }

  it should "serialise, parse and serialise again to the same result" in {
    forAll(events) { sse =>
      val serialised = sse.toString
      ServerSentEvent.parse(lines(serialised)).toString shouldBe serialised
    }
  }
}
