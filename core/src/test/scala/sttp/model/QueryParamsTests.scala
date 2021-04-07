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
}
