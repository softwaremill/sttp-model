package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import sttp.model.{Header, HeaderNames, MediaType}

import scala.collection.immutable.Seq

class AcceptTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  private val acceptHeaderCases = Table(
    ("header", "mediaTypes"),
    ("application/json", Seq((MediaType.ApplicationJson, 1f))),
    ("application/xml;q=1", Seq((MediaType.ApplicationXml, 1f))),
    (
      "text/html;q=0.123;param1=value;param2=value, application/json;q=0.124;param3=value, application/xml;q=0.125",
      Seq((MediaType.ApplicationXml, 0.125f), (MediaType.ApplicationJson, 0.124f), (MediaType.TextHtml, 0.123f))
    ),
    ("text/*;q=0.5, application/xml;q=0.5", Seq((MediaType.ApplicationXml, 0.5f), (MediaType("text", "*"), 0.5f))),
    ("*/*;q=0.55, text/*;q=0.55, text/html;q=0.55", Seq((MediaType.TextHtml, 0.55f), (MediaType("text", "*"), 0.55f), (MediaType("*", "*"), 0.55f)))
  )

  private val otherHeaders = Seq(
    Header(HeaderNames.Authorization, "value"),
    Header(HeaderNames.ContentDisposition, "value"),
    Header(HeaderNames.Connection, "value")
  )

  forAll(acceptHeaderCases) { (header, mediaTypes) =>
    it should s"parse $header" in {
      AcceptHeader.parse(otherHeaders :+ Header(HeaderNames.Accept, header)) shouldBe mediaTypes
    }
  }

  private val acceptCharsetCases = Table(
    ("header", "charsets"),
    ("utf-8", Seq(("utf-8", 1f))),
    ("iso-8859-5;q=1", Seq(("iso-8859-5", 1f))),
    ("iso-8859-5;q=0.5, utf-8;q=0.6", Seq(("utf-8", 0.6f), ("iso-8859-5", 0.5f))),
    ("*;q=0.5, utf-8;q=0.5", Seq(("utf-8", 0.5f), ("*", 0.5f)))
  )

  forAll(acceptCharsetCases) { (header, charsets) =>
    it should s"parse $header" in {
      AcceptCharsetHeader.parse(otherHeaders :+ Header(HeaderNames.AcceptCharset, header)) shouldBe charsets
    }
  }

}
