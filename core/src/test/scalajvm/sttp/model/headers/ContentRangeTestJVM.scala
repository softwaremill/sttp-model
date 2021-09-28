package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ContentRangeTestJVM extends AnyFlatSpec with Matchers {

  it should "fail parsing random string" in {
    val actual = ContentRange.parse("Opuncja")
    actual shouldBe Left("Index 1 out of bounds for length 1")
  }

  it should "fail parsing ContentRange without range and size" in {
    val actual = ContentRange.parse("bytes */*")
    actual shouldBe Left("Predicate does not hold for Content-Range: bytes */*")
  }

  it should "fail parsing ContentRange without incorrect range" in {
    val actual = ContentRange.parse("bytes 555-500/*")
    actual shouldBe Left("Predicate does not hold for Content-Range: bytes 555-500/*")
  }

  it should "fail parsing ContentRange with incorrect range end" in {
    val actual = ContentRange.parse("bytes 5-500/499")
    actual shouldBe Left("Predicate does not hold for Content-Range: bytes 5-500/499")
  }

  it should "fail parsing ContentRange with incorrect range start" in {
    val actual = ContentRange.parse("bytes 555-550/480")
    actual shouldBe Left("Predicate does not hold for Content-Range: bytes 555-550/480")
  }
}
