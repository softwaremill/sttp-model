package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RangeTestJVM extends AnyFlatSpec with Matchers {

  it should "fail parsing random string" in {
    val actual = Range.parse("Opuncja")
    actual shouldBe Left("Index 1 out of bounds for length 1")
  }

  it should "fail parsing header with incorrect range" in {
    val actual = Range.parse("bytes=700-500")
    actual shouldBe Left("Predicate does not hold for List(Range: bytes=700-500)")
  }

  it should "fail parsing header without correct range" in {
    val actual = Range.parse("bytes=-")
    actual shouldBe Left("Index 0 out of bounds for length 0")
  }

  it should "fail for partially correct multiheader" in {
    val actual = Range.parse("bytes=500-700, 900-800")
    actual shouldBe Left("Predicate does not hold for List(Range: bytes=500-700, Range: bytes=900-800)")
  }
}
