package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.model.ContentRangeUnits

class RangeValueTest extends AnyFlatSpec with Matchers {

  it should "return true for valid range" in {
    RangeValue(Some(100), Some(200)).isValid(300) shouldBe true
  }

  it should "return false for invalid range" in {
    RangeValue(Some(100), Some(200)).isValid(100) shouldBe false
  }

  it should "calculate content length" in {
    RangeValue(Some(100), Some(200)).contentLength shouldBe 100
  }

  it should "map RangeValue to content type" in {
    val actual = RangeValue(Some(100), Some(200)).toContentRange(400)
    actual shouldBe ContentRange(ContentRangeUnits.Bytes, Some((100, 200)), Some(400))
  }
}
