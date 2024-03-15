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
}
