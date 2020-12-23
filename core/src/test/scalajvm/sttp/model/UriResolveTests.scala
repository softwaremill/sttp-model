package sttp.model

import Uri._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class UriResolveTests extends AnyFunSuite with Matchers {
  // fails on native for some reason
  test("should resolve relative uris") {
    uri"http://example.org/a/b/c".resolve(uri"../d").toString shouldBe "http://example.org/a/d"
  }
}
