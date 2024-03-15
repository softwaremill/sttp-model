package sttp.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.model.headers.CacheDirective

import java.time.{Instant, LocalDateTime, ZoneId, ZonedDateTime}
import scala.concurrent.duration.DurationInt

class HeaderTests extends AnyFlatSpec with Matchers {
  val rfc1123DatetimeFormatted = "Wed, 08 Feb 2023 02:03:04 GMT"

  val rfc1123DatetimeToBeChecked: Instant = Instant.from {
    ZonedDateTime.of(LocalDateTime.of(2023, 2, 8, 2, 3, 4), ZoneId.of("Z"))
  }

  it should "return a string description of the header" in {
    Header.unsafeApply(HeaderNames.Authorization, "xyz").toString shouldBe "Authorization: xyz"
  }

  it should "validate status codes" in {
    Header.safeApply("Aut ho", "a bc") should matchPattern { case Left(_) => }
    Header.safeApply(HeaderNames.Authorization, "Я ЛЮБЛЮ БОРЩ") should matchPattern { case Left(_) => }
    Header.safeApply(HeaderNames.Authorization, " Я ЛЮБЛЮ БОРЩ") should matchPattern { case Left(_) => }
    Header.safeApply(HeaderNames.Authorization, " xy z") should matchPattern { case Left(_) => }
    Header.safeApply(HeaderNames.Authorization, "xy z\t") should matchPattern { case Left(_) => }
    Header.safeApply(HeaderNames.Authorization, "xy \n z") should matchPattern { case Left(_) => }
    Header.safeApply(HeaderNames.Authorization, " ") should matchPattern { case Left(_) => }
    Header.safeApply(HeaderNames.Authorization, "") should matchPattern { case Right(_) => }
    Header.safeApply(HeaderNames.Authorization, "xy z") should matchPattern { case Right(_) => }
    Header.safeApply(HeaderNames.Authorization, "x \t y  z") should matchPattern { case Right(_) => }
  }

  it should "throw exceptions on invalid headers" in {
    an[IllegalArgumentException] shouldBe thrownBy(Header.unsafeApply("Aut ho", "a bc"))
    an[IllegalArgumentException] shouldBe thrownBy(Header.unsafeApply(HeaderNames.Authorization, " xy z"))
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

  "Instant" should "be formatted according to rfc1123-date1 with a leading zero for single-digit dates" in {
    Header.toHttpDateString(rfc1123DatetimeToBeChecked) shouldBe rfc1123DatetimeFormatted
  }

  "rfc1123-date1" should "be parsed correctly" in {
    Header.parseHttpDate(rfc1123DatetimeFormatted) shouldBe Right(rfc1123DatetimeToBeChecked)
  }

  "rfc850-2-digit-year-after-70" should "be parsed as 19xx" in {
    Header.parseHttpDate("Sun, 08-Feb-70 02:03:04 GMT") shouldBe Right(
      Instant.from {
        ZonedDateTime.of(LocalDateTime.of(1970, 2, 8, 2, 3, 4), ZoneId.of("Z"))
      }
    )
  }

  "rfc850-2-digit-year-before-69" should "be parsed as 20xx" in {
    Header.parseHttpDate("Fri, 08-Feb-69 02:03:04 GMT") shouldBe Right(
      Instant.from {
        ZonedDateTime.of(LocalDateTime.of(2069, 2, 8, 2, 3, 4), ZoneId.of("Z"))
      }
    )
  }

  "rfc850-4-digit-year" should "be parsed correctly" in {
    Header.parseHttpDate("Wed, 08-Feb-2023 02:03:04 GMT") shouldBe Right(
      Instant.from {
        ZonedDateTime.of(LocalDateTime.of(2023, 2, 8, 2, 3, 4), ZoneId.of("Z"))
      }
    )
  }
}
