package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ForwardedTest extends AnyFlatSpec with Matchers {

  it should "parse a single header correctly" in {
    val actual = Forwarded.parse("by=1.2.3.4;for=4.3.2.1;host=example.com;proto=http")
    actual shouldBe Right(List(Forwarded(Some("1.2.3.4"), Some("4.3.2.1"), Some("example.com"), Some("http"))))
  }

  it should "parse a single header correctly, regardless of case of the directives" in {
    val actual = Forwarded.parse("By=1.2.3.4;For=4.3.2.1;Host=example.com;Proto=http")
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

  it should "serialize the header to a string" in {
    val actual =
      Forwarded.toString(List(Forwarded(Some("1.2.3.4"), Some("4.3.2.1"), Some("example.com"), Some("http"))))
    actual shouldBe "by=1.2.3.4;for=4.3.2.1;host=example.com;proto=http"
  }

  it should "serialize multiple headers to a string" in {
    val actual =
      Forwarded.toString(
        List(Forwarded(Some("1.2.3.4"), None, None, None), Forwarded(Some("4.3.2.1"), None, None, None))
      )
    actual shouldBe "by=1.2.3.4,by=4.3.2.1"
  }

  it should "parse ipv6 addresses in 'for', removing the apostrophes" in {
    val actual = Forwarded.parse("""For="[2001:db8:cafe::17]:4711"""")
    actual shouldBe Right(List(Forwarded(None, Some("[2001:db8:cafe::17]:4711"), None, None)))
  }

  it should "parse multiple ipv4 addresses" in {
    val actual = Forwarded.parse("for=192.0.2.43, for=198.51.100.17")
    actual shouldBe Right(
      List(
        Forwarded(None, Some("192.0.2.43"), None, None),
        Forwarded(None, Some("198.51.100.17"), None, None)
      )
    )
  }
}
