package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import sttp.model.ContentTypeRange.AnyRange
import sttp.model.{ContentTypeRange, Header, HeaderNames}

import scala.collection.immutable.Seq

class AcceptsTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  private val acceptsCases = Table(
    ("Accept", "Accept-Charset", "result"),
    ("application/json", "*", Seq(ContentTypeRange("application", "json", "*"))),
    ("application/json", "utf-8", Seq(ContentTypeRange("application", "json", "utf-8"))),
    ("application/json", "utf-8;a=1;b=2;c=3", Seq(ContentTypeRange("application", "json", "utf-8"))),
    (
      "text/plain",
      "utf-8, iso-8559-1",
      Seq(ContentTypeRange("text", "plain", "utf-8"), ContentTypeRange("text", "plain", "iso-8559-1"))
    ),
    (
      "text/plain",
      "utf-8;q=0.9, iso-8559-1",
      Seq(ContentTypeRange("text", "plain", "iso-8559-1"), ContentTypeRange("text", "plain", "utf-8"))
    ),
    (
      "text/plain",
      "utf-8;q=0.5, iso-8559-1;q=0.6, utf-16;q=0.7",
      Seq(
        ContentTypeRange("text", "plain", "utf-16"),
        ContentTypeRange("text", "plain", "iso-8559-1"),
        ContentTypeRange("text", "plain", "utf-8")
      )
    ),
    (
      "text/csv, text/plain",
      "utf-8, utf-16;q=0.5",
      Seq(
        ContentTypeRange("text", "csv", "utf-8"),
        ContentTypeRange("text", "plain", "utf-8"),
        ContentTypeRange("text", "csv", "utf-16"),
        ContentTypeRange("text", "plain", "utf-16")
      )
    ),
    (
      "text/csv, text/plain;q=0.1",
      "utf-8, utf-16;q=0.5",
      Seq(
        ContentTypeRange("text", "csv", "utf-8"),
        ContentTypeRange("text", "csv", "utf-16"),
        ContentTypeRange("text", "plain", "utf-8"),
        ContentTypeRange("text", "plain", "utf-16")
      )
    ),
    (
      "text/*, application/json;q=0.9",
      "utf-8",
      Seq(
        ContentTypeRange("text", "*", "utf-8"),
        ContentTypeRange("application", "json", "utf-8")
      )
    ),
    (
      "*/*",
      "*",
      Seq(AnyRange)
    ),
    (
      "text/plain;q=123",
      "*",
      Seq(ContentTypeRange("text", "plain", "*"))
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
    Accepts.unsafeParse(otherHeaders) shouldBe Seq(AnyRange)
  }

  it should "parse when only Accept-Charset specified" in {
    Accepts.unsafeParse(otherHeaders ++ Seq(Header(HeaderNames.AcceptCharset, "utf-16;q=0.5, utf-8"))) shouldBe Seq(
      ContentTypeRange("*", "*", "utf-8"),
      ContentTypeRange("*", "*", "utf-16")
    )
  }

  it should "parse when only Accept specified" in {
    Accepts.unsafeParse(otherHeaders ++ Seq(Header(HeaderNames.Accept, "text/csv;q=0.1, text/plain"))) shouldBe Seq(
      ContentTypeRange("text", "plain", "*"),
      ContentTypeRange("text", "csv", "*")
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
