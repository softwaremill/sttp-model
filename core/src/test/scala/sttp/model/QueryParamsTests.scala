package sttp.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QueryParamsTests extends AnyFlatSpec with Matchers {
  it should "support no-value parameters" in {
    val qp = QueryParams.fromMultiSeq(List(("p", Nil), ("q", List("1")), ("r", List("1", "2"))))
    qp.getMulti("p") shouldBe Some(Nil)
    qp.getMulti("q") shouldBe Some(List("1"))
    qp.getMulti("r") shouldBe Some(List("1", "2"))
    qp.getMulti("s") shouldBe None
  }

  it should "serialize to string" in {
    val qp = QueryParams.fromMultiSeq(List(("p", Nil), ("q", List("1")), ("r", List("1", "2"))))
    qp.toString shouldBe "p&q=1&r=1&r=2"
  }

  it should "serialize to string, encoding special character" in {
    val qp = QueryParams.fromSeq(List(("p", "a b")))
    qp.toString shouldBe "p=a+b"
  }

  it should "serialize to string with a boundary if there are query params" in {
    val qp = QueryParams.fromSeq(List(("p", "1"), ("q", "2")))
    qp.toString(includeBoundary = true) shouldBe "?p=1&q=2"
  }

  it should "serialize to string without boundary if there are no query params" in {
    val qp = QueryParams.fromSeq(Nil)
    qp.toString(includeBoundary = true) shouldBe ""
  }
}
