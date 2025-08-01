package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.model.ContentRangeUnits

class ContentRangeTest extends AnyFlatSpec with Matchers {

  it should "properly parse simplest ContentRange header" in {
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
    ContentRange.parse("Opuncja") shouldBe Left(
      "Expected content-range in the format: \"unit range/size\", but got: Opuncja"
    )
  }

  it should "fail parsing ContentRange without range and size" in {
    ContentRange.parse("bytes */*") shouldBe Left("Invalid Content-Range")
  }

  it should "fail parsing ContentRange with invalid range end" in {
    ContentRange.parse("bytes 200-abc/*") shouldBe Left("Invalid end of range: abc")
  }

  it should "fail parsing ContentRange with invalid range start" in {
    ContentRange.parse("bytes abc-200/*") shouldBe Left("Invalid start of range: abc")
  }

  it should "fail parsing ContentRange with missing range start" in {
    ContentRange.parse("bytes -200/*") shouldBe Left("Invalid start of range: ")
  }

  it should "fail parsing ContentRange with missing range end" in {
    ContentRange.parse("bytes 200-/*") shouldBe Left("Invalid range: 200-")
  }

  it should "fail parsing ContentRange with incorrect range" in {
    ContentRange.parse("bytes 555-500/*") shouldBe Left("Invalid Content-Range")
  }

  it should "fail parsing ContentRange with incorrect range end" in {
    ContentRange.parse("bytes 5-500/499") shouldBe Left("Invalid Content-Range")
  }

  it should "fail parsing ContentRange with incorrect range start" in {
    ContentRange.parse("bytes 555-550/480") shouldBe Left("Invalid Content-Range")
  }
}
