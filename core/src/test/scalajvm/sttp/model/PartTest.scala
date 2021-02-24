package sttp.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PartTest extends AnyFlatSpec with Matchers {

  val newCharset = "ISO-8859-1"

  it should "add charset to contentType" in {
    val part: Part[String] = Part[String]("p1", "v1", contentType = Some(MediaType.TextPlain))

    val result = part.addCharsetToContentType(newCharset)

    result.contentType shouldBe Some(s"text/plain; charset=$newCharset")
  }

  it should "do not change contentType if it already contains charset" in {
    val part: Part[String] = Part[String]("p1", "v1", contentType = Some(MediaType.TextPlainUtf8))

    val result = part.addCharsetToContentType(newCharset)

    result.contentType shouldBe Some("text/plain; charset=utf-8")
  }

  it should "do not add contentType if it not exists" in {
    val part: Part[String] = Part[String]("p1", "v1")

    val result = part.addCharsetToContentType(newCharset)

    result.contentType shouldBe None
  }
}
