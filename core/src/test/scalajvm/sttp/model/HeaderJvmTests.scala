package sttp.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

// TODO: collapse into HeaderTests once native support java time
class HeaderJvmTests extends AnyFlatSpec with Matchers {
  it should "properly format expires date" in {
    val i = ZonedDateTime.parse("Wed, 21 Oct 2015 07:28:00 GMT", DateTimeFormatter.RFC_1123_DATE_TIME)
    Header.expires(i.toInstant).toString shouldBe "Expires: Wed, 21 Oct 2015 07:28:00 GMT"
  }
}
