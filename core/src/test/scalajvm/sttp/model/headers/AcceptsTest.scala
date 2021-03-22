package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import sttp.model.MediaType.Wildcard
import sttp.model.{Header, HeaderNames, MediaType}

import scala.collection.immutable.Seq

class AcceptsTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  private val acceptsCases = Table(
    ("Accept", "Accept-Charset", "result"),
    ("application/json", "*", Seq(MediaType.ApplicationJson.charset("*"))),
    ("application/json", "utf-8", Seq(MediaType.ApplicationJson.charset("utf-8"))),
    ("application/json", "utf-8;a=1;b=2;c=3", Seq(MediaType.ApplicationJson.charset("utf-8"))),
    (
      "text/plain",
      "utf-8, iso-8559-1",
      Seq(MediaType.TextPlain.charset("utf-8"), MediaType.TextPlain.charset("iso-8559-1"))
    ),
    (
      "text/plain",
      "utf-8;q=0.9, iso-8559-1",
      Seq(MediaType.TextPlain.charset("iso-8559-1"), MediaType.TextPlain.charset("utf-8"))
    ),
    (
      "text/plain",
      "utf-8;q=0.5, iso-8559-1;q=0.6, utf-16;q=0.7",
      Seq(
        MediaType.TextPlain.charset("utf-16"),
        MediaType.TextPlain.charset("iso-8559-1"),
        MediaType.TextPlain.charset("utf-8")
      )
    ),
    (
      "text/csv, text/plain",
      "utf-8, utf-16;q=0.5",
      Seq(
        MediaType.TextCsv.charset("utf-8"),
        MediaType.TextPlain.charset("utf-8"),
        MediaType.TextCsv.charset("utf-16"),
        MediaType.TextPlain.charset("utf-16")
      )
    ),
    (
      "text/csv, text/plain;q=0.1",
      "utf-8, utf-16;q=0.5",
      Seq(
        MediaType.TextCsv.charset("utf-8"),
        MediaType.TextCsv.charset("utf-16"),
        MediaType.apply("text", "plain", Some("utf-8"), Map("q" -> "0.1")),
        MediaType.apply("text", "plain", Some("utf-16"), Map("q" -> "0.1"))
      )
    ),
    (
      "text/*, application/json;q=0.9",
      "utf-8",
      Seq(
        MediaType("text", "*", Some("utf-8")),
        MediaType.ApplicationJson.charset("utf-8").copy(parameters = Map("q" -> "0.9"))
      )
    )
  )

  private val otherHeaders = Seq(
    Header(HeaderNames.Authorization, "value"),
    Header(HeaderNames.ContentDisposition, "value"),
    Header(HeaderNames.Connection, "value")
  )

  forAll(acceptsCases) { (accept, acceptCharset, result) =>
    it should s"parse Accept: $accept Accept-Charset: $acceptCharset" in {
      val headers =
        otherHeaders ++ Seq(Header(HeaderNames.Accept, accept), Header(HeaderNames.AcceptCharset, acceptCharset))
      Accepts.unsafeParse(headers) shouldBe result
      Accepts.parse(headers) shouldBe Right(result)
    }
  }

  it should "parse to any when no accept headers" in {
    Accepts.unsafeParse(otherHeaders) shouldBe Seq(MediaType(Wildcard, Wildcard))
  }

  it should "parse when only Accept-Charset specified" in {
    Accepts.unsafeParse(otherHeaders ++ Seq(Header(HeaderNames.AcceptCharset, "utf-16;q=0.5, utf-8"))) shouldBe Seq(
      MediaType("*", "*", Some("utf-8")),
      MediaType("*", "*", Some("utf-16"))
    )
  }

  it should "parse when only Accept specified" in {
    Accepts.unsafeParse(otherHeaders ++ Seq(Header(HeaderNames.Accept, "text/csv;q=0.1, text/plain"))) shouldBe Seq(
      MediaType.TextPlain,
      MediaType.TextCsv.copy(parameters = Map("q" -> "0.1"))
    )
  }

  it should "return errors when invalid accept headers" in {
    val invalidAccept = Header(HeaderNames.Accept, "text/html;=q=1")
    Accepts.parse(Seq(invalidAccept)) shouldBe Left(
      """Parameter is not formatted correctly: "=q=1" for: "text/html;=q=1""""
    )

    val invalidAcceptCharset = Header(HeaderNames.AcceptCharset, "utf-8;=a=1")
    Accepts.parse(Seq(invalidAcceptCharset)) shouldBe Left(
      """Parameter is not formatted correctly: "=a=1" for: "utf-8;=a=1""""
    )

    Accepts.parse(Seq(invalidAccept, invalidAcceptCharset)) shouldBe Left(
      """Parameter is not formatted correctly: "=q=1" for: "text/html;=q=1"
        |Parameter is not formatted correctly: "=a=1" for: "utf-8;=a=1"""".stripMargin
    )
  }
}
