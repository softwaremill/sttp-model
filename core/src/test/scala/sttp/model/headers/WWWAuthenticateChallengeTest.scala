package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WWWAuthenticateChallengeTest extends AnyFlatSpec with Matchers {

  it should "properly serialise a basic header value" in {
    WWWAuthenticateChallenge.basic.toString shouldBe "Basic"
  }

  it should "properly serialise a basic header value with realm" in {
    WWWAuthenticateChallenge.basic("xyz").toString shouldBe "Basic realm=\"xyz\""
  }

  it should "properly serialise a header value with params" in {
    WWWAuthenticateChallenge("Digest")
      .realm("http-auth@example.org")
      .addParam("qop", "auth, auth-int")
      .addParam("nonce", "7ypf/xlj9XXwfDPEoM4URrv/xwf94BcCAzFZH4GiTo0v")
      .toString shouldBe "Digest realm=\"http-auth@example.org\", qop=\"auth, auth-int\", nonce=\"7ypf/xlj9XXwfDPEoM4URrv/xwf94BcCAzFZH4GiTo0v\""
  }
}
