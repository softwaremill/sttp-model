package sttp.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.concurrent.duration.DurationInt

class HeaderTests extends AnyFlatSpec with Matchers {
  it should "return a string description of the header" in {
    Header.unsafeApply(HeaderNames.Authorization, "xyz").toString shouldBe "Authorization: xyz"
  }

  it should "validate status codes" in {
    Header.safeApply("Aut ho", "a bc") shouldBe Symbol("left")
    Header.safeApply(HeaderNames.Authorization, "xy z") shouldBe Symbol("right")
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
    Header.cacheControl(maxAge = Some(3.seconds)).toString shouldBe "Cache-Control: max-age=3"
    Header.cacheControl(noCache = true).toString shouldBe "Cache-Control: no-cache"
    Header.cacheControl(maxStale = Some(None)).toString shouldBe "Cache-Control: max-stale"
    Header.cacheControl(maxStale = Some(Some(1.second))).toString shouldBe "Cache-Control: max-stale=1"
    Header
      .cacheControl(noTransform = true, public = true, sMaxage = Some(10.seconds))
      .toString shouldBe "Cache-Control: no-transform, public, s-maxage=10"
  }

  it should "properly quote an etag" in {
    Header.etag("xyz").toString shouldBe "ETag: \"xyz\""
  }

  it should "properly quote a weak etag" in {
    Header.etag("xyz", weak = true).toString shouldBe "ETag: W/\"xyz\""
  }

  it should "properly format expires date" in {
    val i = ZonedDateTime.parse("Wed, 21 Oct 2015 07:28:00 GMT", DateTimeFormatter.RFC_1123_DATE_TIME)
    Header.expires(i.toInstant).toString shouldBe "Expires: Wed, 21 Oct 2015 07:28:00 GMT"
  }
}
