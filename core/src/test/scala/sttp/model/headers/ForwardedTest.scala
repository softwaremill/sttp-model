package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ForwardedTest extends AnyFlatSpec with Matchers {

  it should "parse a single header correctly" in {
    val actual = Forwarded.parse("by=1.2.3.4;for=4.3.2.1;host=example.com;proto=http")
    actual shouldBe Right(List(Forwarded(Some("1.2.3.4"), Some("4.3.2.1"), Some("example.com"), Some("http"))))
  }

  it should "parse multiple headers correctly, when given as separate headers" in {
    val actual = Forwarded.parse(List("by=1.2.3.4;for=4.3.2.1", "host=example.com;proto=https"))
    actual shouldBe Right(
      List(
        Forwarded(Some("1.2.3.4"), Some("4.3.2.1"), None, None),
        Forwarded(None, None, Some("example.com"), Some("https"))
      )
    )
  }

  it should "parse multiple headers correctly, when given as a single header" in {
    val actual = Forwarded.parse(List("by=1.2.3.4;for=4.3.2.1, host=example.com;proto=https"))
    actual shouldBe Right(
      List(
        Forwarded(Some("1.2.3.4"), Some("4.3.2.1"), None, None),
        Forwarded(None, None, Some("example.com"), Some("https"))
      )
    )
  }

  it should "handle missing fields" in {
    val actual = Forwarded.parse("by=1.2.3.4;host=example.com")
    actual shouldBe Right(List(Forwarded(Some("1.2.3.4"), None, Some("example.com"), None)))
  }

  it should "return an error for invalid headers" in {
    val actual = Forwarded.parse("by=1.2.3.4;invalid")
    actual shouldBe Left("Invalid part: invalid")
  }

  it should "return an error for invalid key-value pairs" in {
    val actual = Forwarded.parse("by=1.2.3.4;for")
    actual shouldBe Left("Invalid part: for")
  }
}
