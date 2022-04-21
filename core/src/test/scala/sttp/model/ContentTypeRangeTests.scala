package sttp.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ContentTypeRangeTests extends AnyFlatSpec with Matchers {
  it should "serialize using toString" in {
    ContentTypeRange.AnyRange.toString shouldBe "*/*"
    ContentTypeRange.AnyText.toString shouldBe "text/*"
    ContentTypeRange.exact(MediaType.TextPlainUtf8).toString shouldBe "text/plain; charset=utf-8"
    ContentTypeRange.exactNoCharset(MediaType.TextPlainUtf8).toString shouldBe "text/plain"
  }

  it should "modify the range to any sub type" in {
    ContentTypeRange.exact(MediaType.TextPlainUtf8).anySubType.toString shouldBe "text/*; charset=utf-8"
    ContentTypeRange.exact(MediaType.TextPlain).anySubType.toString shouldBe "text/*"
  }

  it should "modify the range to any charset" in {
    ContentTypeRange.exact(MediaType.TextPlainUtf8).anyCharset.toString shouldBe "text/plain"
  }

  it should "be case-insensitive when comparing instances" in {
    ContentTypeRange.AnyRange.equals(ContentTypeRange.AnyRange) shouldBe true

    ContentTypeRange
      .exact(MediaType.TextPlainUtf8)
      .equals(ContentTypeRange.exact(MediaType.TextPlainUtf8).copy(mainType = "TEXT")) shouldBe true
    ContentTypeRange
      .exact(MediaType.TextPlainUtf8)
      .equals(ContentTypeRange.exact(MediaType.TextPlainUtf8).copy(subType = "PLAIN")) shouldBe true

    ContentTypeRange.exact(MediaType.TextPlain).equals(ContentTypeRange.exact(MediaType.TextHtml)) shouldBe false
    ContentTypeRange
      .exactNoCharset(MediaType.TextPlain)
      .equals(ContentTypeRange.exact(MediaType.TextHtml)) shouldBe false
  }
}
