package sttp.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PartTest extends AnyFlatSpec with Matchers {
  it should "encode content-disposition header correctly" in {
    val part = Part("name", 42).fileName("f1.txt")
    part.contentDispositionHeaderValue shouldBe """form-data; name="name"; filename="f1.txt""""
  }

  it should "retain non-ascii characters in filename" in {
    val part = Part("name", 42).fileName("fó1.txt")
    part.contentDispositionHeaderValue shouldBe """form-data; name="name"; filename="fó1.txt""""
  }

  it should "retain non-ascii characters in filename*" in {
    val expectedFileName = "fó1.txt"
    val part = Part("name", 42, otherDispositionParams = Map("filename*" -> expectedFileName)).fileName("f1.txt")
    part.contentDispositionHeaderValue shouldBe s"""form-data; name="name"; filename*="$expectedFileName"; filename="f1.txt""""
    part.fileName shouldBe Some(expectedFileName)
  }

  it should "fallback to basic filename if filename* is not present" in {
    val part = Part("name", 42).fileName("f1.txt")
    part.contentDispositionHeaderValue shouldBe s"""form-data; name="name"; filename="f1.txt""""
    part.fileName shouldBe Some("f1.txt")
  }

  it should "escape double quote and backslash in filename" in {
    val part = Part("name", 42).fileName("f\"\\1.txt")
    part.contentDispositionHeaderValue shouldBe """form-data; name="name"; filename="f\"\\1.txt""""
  }
}
