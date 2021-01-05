package sttp.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.model.headers.CacheDirective

import scala.concurrent.duration.DurationInt

class HeaderTests extends AnyFlatSpec with Matchers {
  it should "return a string description of the header" in {
    Header.unsafeApply(HeaderNames.Authorization, "xyz").toString shouldBe "Authorization: xyz"
  }

  it should "validate status codes" in {
    Header.safeApply("Aut ho", "a bc") should matchPattern { case Left(_) => }
    Header.safeApply(HeaderNames.Authorization, "xy z") should matchPattern { case Right(_) => }
  }

  it should "throw exceptions on invalid headers" in {
    an[IllegalArgumentException] shouldBe thrownBy(Header.unsafeApply("Aut ho", "a bc"))
  }

  it should "create unvalidated instances" in {
    Header("Aut ho", "a bc").toString shouldBe "Aut ho: a bc"
  }

  it should "return a safe string description of a sensitive header" in {
    val h = Header.authorization("bearer", "123")
    h.toString shouldBe "Authorization: bearer 123"
    h.toStringSafe() shouldBe "Authorization: ***"
  }

  it should "properly create a cache-control header" in {
    Header
      .cacheControl(CacheDirective.NoTransform, CacheDirective.Public, CacheDirective.SMaxage(10.seconds))
      .toString shouldBe "Cache-Control: no-transform, public, s-maxage=10"
  }
}
