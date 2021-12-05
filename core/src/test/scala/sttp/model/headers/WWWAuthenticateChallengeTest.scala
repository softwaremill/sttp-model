package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.ListMap

class WWWAuthenticateChallengeTest extends AnyFlatSpec with Matchers {

  it should "properly serialise a basic header value" in {
    WWWAuthenticateChallenge.parseSingle("Basic") shouldBe Right(WWWAuthenticateChallenge("Basic"))
  }

  it should "properly serialise a basic header value with realm" in {
    val actual = WWWAuthenticateChallenge.parseSingle("Basic realm=\"xyz\"")
    actual shouldBe Right(WWWAuthenticateChallenge("Basic", ListMap("realm" -> "xyz")))
  }

  it should "properly serialise a basic header value with realm and charset" in {
    val actual = WWWAuthenticateChallenge.parseSingle("Basic realm=\"xyz\", charset=\"UTF-8\"")
    actual shouldBe Right(WWWAuthenticateChallenge("Basic", ListMap("realm" -> "xyz", "charset" -> "UTF-8")))
  }

  it should "properly serialise a basic header value with charset" in {
    val actual = WWWAuthenticateChallenge.parseSingle("Basic charset=\"UTF-8\"")
    actual shouldBe Right(WWWAuthenticateChallenge("Basic", ListMap("charset" -> "UTF-8")))
  }

  it should "properly serialise a bearer header value with realm" in {
    val actual = WWWAuthenticateChallenge.parseSingle("Bearer realm=\"xyz\"")
    actual shouldBe Right(WWWAuthenticateChallenge("Bearer", ListMap("realm" -> "xyz")))
  }

  it should "properly serialise a header value with params" in {
    val actual = WWWAuthenticateChallenge.parseSingle("Digest realm=\"http-auth@example.org\", qop=\"auth, auth-int\", nonce=\"xxxx\"")
    val params = ListMap("realm" -> "http-auth@example.org", "qop" -> "auth, auth-int", "nonce" -> "xxxx")
    actual shouldBe Right(WWWAuthenticateChallenge("Digest", params))
  }

  it should "properly serialise a Digest challenge" in {
    val actual = WWWAuthenticateChallenge.parseSingle("Digest realm=\"http-auth@example.org\", qop=\"auth, auth-int\", nonce=\"xxxx\", algorithm=\"MD5\"")
    val params = ListMap("realm" -> "http-auth@example.org", "qop" -> "auth, auth-int", "nonce" -> "xxxx", "algorithm" -> "MD5")
    actual shouldBe Right(WWWAuthenticateChallenge("Digest", params))
  }
}
