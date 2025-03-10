package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HostTest extends AnyFlatSpec with Matchers {
  it should "parse host and port" in {
    Host.parseHostAndPort("example.com:8080") shouldBe (("example.com", Some(8080)))
  }

  it should "parse host without port" in {
    Host.parseHostAndPort("example.com") shouldBe (("example.com", None))
  }

  it should "parse IPv4 address with port" in {
    Host.parseHostAndPort("192.168.1.1:8080") shouldBe (("192.168.1.1", Some(8080)))
  }

  it should "parse IPv4 address without port" in {
    Host.parseHostAndPort("192.168.1.1") shouldBe (("192.168.1.1", None))
  }

  it should "parse IPv6 address with port" in {
    Host.parseHostAndPort("[2001:db8::1]:8080") shouldBe (("2001:db8::1", Some(8080)))
  }

  it should "parse IPv6 address without port" in {
    Host.parseHostAndPort("[2001:db8::1]") shouldBe (("2001:db8::1", None))
  }

}
