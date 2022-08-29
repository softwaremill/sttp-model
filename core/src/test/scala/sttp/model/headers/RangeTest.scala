package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.model.ContentRangeUnits

class RangeTest extends AnyFlatSpec with Matchers {

  it should "properly parse the Range header of length one" in {
    val actual = Range.parse("bytes=10-10")
    actual shouldBe Right(List(Range(Some(10), Some(10), ContentRangeUnits.Bytes)))
  }

  it should "properly parse simplest Range header" in {
    val actual = Range.parse("bytes=200-1000")
    actual shouldBe Right(List(Range(Some(200), Some(1000), ContentRangeUnits.Bytes)))
  }

  it should "properly parse Range without start" in {
    val actual = Range.parse("bytes=-1000")
    actual shouldBe Right(List(Range(None, Some(1000), ContentRangeUnits.Bytes)))
  }

  it should "properly parse Range without end" in {
    val actual = Range.parse("bytes=200-")
    actual shouldBe Right(List(Range(Some(200), None, ContentRangeUnits.Bytes)))
  }

  it should "properly parse Range simple multirange" in {
    val actual = Range.parse("bytes=200-1000, 1200-1400")
    val expectedHeaders = List(
      Range(Some(200), Some(1000), ContentRangeUnits.Bytes),
      Range(Some(1200), Some(1400), ContentRangeUnits.Bytes)
    )
    actual shouldBe Right(expectedHeaders)
  }

  it should "properly parse Range mutltirange without end" in {
    val actual = Range.parse("bytes=200-1000, 1200-")
    val expectedHeaders =
      List(Range(Some(200), Some(1000), ContentRangeUnits.Bytes), Range(Some(1200), None, ContentRangeUnits.Bytes))
    actual shouldBe Right(expectedHeaders)
  }

  it should "properly parse Range mutltirange without start" in {
    val actual = Range.parse("bytes=200-1000, -1400")
    val expectedHeaders =
      List(Range(Some(200), Some(1000), ContentRangeUnits.Bytes), Range(None, Some(1400), ContentRangeUnits.Bytes))
    actual shouldBe Right(expectedHeaders)
  }

  it should "properly parse Range mutltirange(3) without end" in {
    val actual = Range.parse("bytes=400-1600, 1800-1900, 2100-")
    val expectedHeaders = List(
      Range(Some(400), Some(1600), ContentRangeUnits.Bytes),
      Range(Some(1800), Some(1900), ContentRangeUnits.Bytes),
      Range(Some(2100), None, ContentRangeUnits.Bytes)
    )
    actual shouldBe Right(expectedHeaders)
  }

  it should "properly parse Range mutltirange(3) without start" in {
    val actual = Range.parse("bytes=500-700, 900-1000, -1300")
    val expectedHeaders = List(
      Range(Some(500), Some(700), ContentRangeUnits.Bytes),
      Range(Some(900), Some(1000), ContentRangeUnits.Bytes),
      Range(None, Some(1300), ContentRangeUnits.Bytes)
    )
    actual shouldBe Right(expectedHeaders)
  }

  it should "fail parsing random string" in {
    Range.parse("Opuncja") shouldBe Left("Expected range in the format: \"unit=start/end\", but got: Opuncja")
  }

  it should "fail parsing header with incorrect range" in {
    Range.parse("bytes=700-500") shouldBe Left("Invalid Range")
  }

  it should "fail parsing header without correct range" in {
    Range.parse("bytes=-") shouldBe Left("Invalid Range")
  }

  it should "fail for partially correct multiheader" in {
    Range.parse("bytes=500-700, 900-800") shouldBe Left("Invalid Range")
  }

  it should "return true for valid range" in {
    Range(Some(100), Some(200), ContentRangeUnits.Bytes).isValid(300) shouldBe true
  }

  it should "return false for invalid range" in {
    Range(Some(100), Some(200), ContentRangeUnits.Bytes).isValid(100) shouldBe false
  }

  it should "return true for range with only start" in {
    Range(Some(200), None, ContentRangeUnits.Bytes).isValid(400) shouldBe true
  }

  it should "return false for incorrect range with only start" in {
    Range(Some(200), None, ContentRangeUnits.Bytes).isValid(150) shouldBe false
  }

  it should "return true for range with only end" in {
    Range(None, Some(500), ContentRangeUnits.Bytes).isValid(600) shouldBe true
  }

  it should "return false for incorrect range with only end" in {
    Range(None, Some(500), ContentRangeUnits.Bytes).isValid(350) shouldBe false
  }

  it should "return false for range without start and end" in {
    Range(None, None, ContentRangeUnits.Bytes).isValid(250) shouldBe false
  }

  it should "calculate content length" in {
    Range(Some(100), Some(200), ContentRangeUnits.Bytes).contentLength shouldBe 101
  }

  it should "map RangeValue to content type" in {
    val actual = Range(Some(100), Some(200), ContentRangeUnits.Bytes).toContentRange(400)
    actual shouldBe ContentRange(ContentRangeUnits.Bytes, Some((100, 200)), Some(400))
  }
}
