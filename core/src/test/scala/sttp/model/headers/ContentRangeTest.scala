package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.model.ContentRangeUnits

class ContentRangeTest extends AnyFlatSpec with Matchers {

  it should "properly pars simplest ContentRange header" in {
    val actual = ContentRange.parse("bytes 200-1000/1200")
    actual shouldBe Right(ContentRange(ContentRangeUnits.Bytes, Some((200, 1000)), Some(1200)))
  }

  it should "properly parse ContentRange without size" in {
    val actual = ContentRange.parse("bytes 555-990/*")
    actual shouldBe Right(ContentRange(ContentRangeUnits.Bytes, Some((555, 990)), None))
  }

  it should "properly parse ContentRange without range" in {
    val actual = ContentRange.parse("bytes */300")
    actual shouldBe Right(ContentRange(ContentRangeUnits.Bytes, None, Some(300)))
  }

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
