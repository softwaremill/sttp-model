package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.model.ContentRangeUnits

class RangeHeaderTest extends AnyFlatSpec with Matchers {

  it should "properly pars simplest Range header" in {
    val actual = RangeHeader.parse("bytes=200-1000")
    actual shouldBe Right(List(RangeHeader(Some(200), Some(1000), ContentRangeUnits.Bytes)))
  }

  it should "properly parse Range without star" in {
    val actual = RangeHeader.parse("bytes=-1000")
    actual shouldBe Right(List(RangeHeader(None, Some(1000), ContentRangeUnits.Bytes)))
  }

  it should "properly parse Range without end" in {
    val actual = RangeHeader.parse("bytes=200-")
    actual shouldBe Right(List(RangeHeader(Some(200), None, ContentRangeUnits.Bytes)))
  }

  it should "properly parse Range simple multirange" in {
    val actual = RangeHeader.parse("bytes=200-1000, 1200-1400")
    val expectedHeaders = List(
      RangeHeader(Some(200), Some(1000), ContentRangeUnits.Bytes),
      RangeHeader(Some(1200), Some(1400), ContentRangeUnits.Bytes))
    actual shouldBe Right(expectedHeaders)
  }

  it should "properly parse Range mutltirange without end" in {
    val actual = RangeHeader.parse("bytes=200-1000, 1200-")
    val expectedHeaders = List(
      RangeHeader(Some(200), Some(1000), ContentRangeUnits.Bytes),
      RangeHeader(Some(1200), None, ContentRangeUnits.Bytes))
    actual shouldBe Right(expectedHeaders)
  }

  it should "properly parse Range mutltirange without start" in {
    val actual = RangeHeader.parse("bytes=200-1000, -1400")
    val expectedHeaders = List(
      RangeHeader(Some(200), Some(1000), ContentRangeUnits.Bytes),
      RangeHeader(None, Some(1400), ContentRangeUnits.Bytes))
    actual shouldBe Right(expectedHeaders)
  }

  it should "properly parse Range mutltirange(3) without end" in {
    val actual = RangeHeader.parse("bytes=400-1600, 1800-1900, 2100-")
    val expectedHeaders = List(
      RangeHeader(Some(400), Some(1600), ContentRangeUnits.Bytes),
      RangeHeader(Some(1800), Some(1900), ContentRangeUnits.Bytes),
      RangeHeader(Some(2100), None, ContentRangeUnits.Bytes))
    actual shouldBe Right(expectedHeaders)
  }

  it should "properly parse Range mutltirange(3) without start" in {
    val actual = RangeHeader.parse("bytes=500-700, 900-1000, -1300")
    val expectedHeaders = List(
      RangeHeader(Some(500), Some(700), ContentRangeUnits.Bytes),
      RangeHeader(Some(900), Some(1000), ContentRangeUnits.Bytes),
      RangeHeader(None, Some(1300), ContentRangeUnits.Bytes))
    actual shouldBe Right(expectedHeaders)
  }

  it should "fail parsing random string" in {
    val actual = RangeHeader.parse("Opuncja")
    actual shouldBe Left("Index 1 out of bounds for length 1")
  }

  it should "fail parsing header with incorrect range" in {
    val actual = RangeHeader.parse("bytes=700-500")
    actual shouldBe Left("Predicate does not hold for List(Range: bytes=700-500)")
  }

  it should "fail parsing header without correct range" in {
    val actual = RangeHeader.parse("bytes=-")
    actual shouldBe Left("Index 0 out of bounds for length 0")
  }

  it should "fail for partially correct multiheader" in {
    val actual = RangeHeader.parse("bytes=500-700, 900-800")
    actual shouldBe Left("Predicate does not hold for List(Range: bytes=500-700, Range: bytes=900-800)")
  }
}
