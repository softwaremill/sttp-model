package sttp.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MethodTest extends AnyFlatSpec with Matchers {
  "unsafeApply" should "not be case sensitive" in {
    Method.unsafeApply("get") shouldBe Method.unsafeApply("GET")
    Method.unsafeApply("head") shouldBe Method.unsafeApply("HEAD")
    Method.unsafeApply("post") shouldBe Method.unsafeApply("POST")
    Method.unsafeApply("put") shouldBe Method.unsafeApply("PUT")
    Method.unsafeApply("delete") shouldBe Method.unsafeApply("DELETE")
    Method.unsafeApply("options") shouldBe Method.unsafeApply("OPTIONS")
    Method.unsafeApply("patch") shouldBe Method.unsafeApply("PATCH")
    Method.unsafeApply("connect") shouldBe Method.unsafeApply("CONNECT")
    Method.unsafeApply("trace") shouldBe Method.unsafeApply("TRACE")
  }
}
