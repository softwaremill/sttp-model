package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.model.ContentRangeUnits

class RangeTest extends AnyFlatSpec with Matchers {

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
      Range(Some(1200), Some(1400), ContentRangeUnits.Bytes))
    actual shouldBe Right(expectedHeaders)
  }

  it should "properly parse Range mutltirange without end" in {
    val actual = Range.parse("bytes=200-1000, 1200-")
    val expectedHeaders = List(
      Range(Some(200), Some(1000), ContentRangeUnits.Bytes),
      Range(Some(1200), None, ContentRangeUnits.Bytes))
    actual shouldBe Right(expectedHeaders)
  }

  it should "properly parse Range mutltirange without start" in {
    val actual = Range.parse("bytes=200-1000, -1400")
    val expectedHeaders = List(
      Range(Some(200), Some(1000), ContentRangeUnits.Bytes),
      Range(None, Some(1400), ContentRangeUnits.Bytes))
    actual shouldBe Right(expectedHeaders)
  }

  it should "properly parse Range mutltirange(3) without end" in {
    val actual = Range.parse("bytes=400-1600, 1800-1900, 2100-")
    val expectedHeaders = List(
      Range(Some(400), Some(1600), ContentRangeUnits.Bytes),
      Range(Some(1800), Some(1900), ContentRangeUnits.Bytes),
      Range(Some(2100), None, ContentRangeUnits.Bytes))
    actual shouldBe Right(expectedHeaders)
  }

  it should "properly parse Range mutltirange(3) without start" in {
    val actual = Range.parse("bytes=500-700, 900-1000, -1300")
    val expectedHeaders = List(
      Range(Some(500), Some(700), ContentRangeUnits.Bytes),
      Range(Some(900), Some(1000), ContentRangeUnits.Bytes),
      Range(None, Some(1300), ContentRangeUnits.Bytes))
    actual shouldBe Right(expectedHeaders)
  }
}
