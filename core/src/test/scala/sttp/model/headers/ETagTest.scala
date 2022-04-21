package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ETagTest extends AnyFlatSpec with Matchers {
  it should "properly quote an etag" in {
    ETag("xyz").toString shouldBe "\"xyz\""
  }

  it should "properly quote a list of etags" in {
    ETag.toString(List(ETag("xyz"), ETag("abc"), ETag("123"))) shouldBe "\"xyz\", \"abc\", \"123\""
  }

  it should "properly quote a weak etag" in {
    ETag("xyz", weak = true).toString shouldBe "W/\"xyz\""
  }

  it should "parse an etag" in {
    ETag.parse("\"xyz\"") shouldBe Right(ETag("xyz"))
  }

  it should "parse a list of etags" in {
    ETag.parseList("\"xyz\", \"abc\", \"123\"") shouldBe Right(List(ETag("xyz"), ETag("abc"), ETag("123")))
  }

  it should "parse a list of etags with weak etags" in {
    ETag.parseList("\"xyz\", W/\"abc\", \"123\"") shouldBe Right(
      List(ETag("xyz"), ETag("abc", weak = true), ETag("123"))
    )
  }

  it should "parse a weak etag" in {
    ETag.parse("W/\"xyz\"") shouldBe Right(ETag("xyz", weak = true))
  }

  it should "not parse an invalid etag" in {
    ETag.parse("\"xyz").isLeft shouldBe true
  }
}
