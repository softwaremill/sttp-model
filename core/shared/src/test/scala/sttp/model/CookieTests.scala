package sttp.model

import java.time.Instant

import org.scalatest.TryValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CookieTests extends AnyFunSuite with Matchers with TryValues {
  test("parse - should parse a cookie with unknown attributes") {
    val cookieRawValue =
      "WELCOME_STATE_CHECKER=Xh3JYGEWrY7X2CIyI8_xqJ4UkipYtamLS2YOU27h8vY; Version=1; Something-Else; expires=Tue, 28-Jan-2020 17:13:10 GMT; max-Age=300; Path=/; HttpOnly"

    val result = CookieWithMeta.parse(cookieRawValue)

    result shouldBe Right(
      CookieWithMeta(
        "WELCOME_STATE_CHECKER",
        CookieValueWithMeta(
          "Xh3JYGEWrY7X2CIyI8_xqJ4UkipYtamLS2YOU27h8vY",
          expires = Some(Instant.ofEpochMilli(1580231590000L)),
          maxAge = Some(300),
          domain = None,
          path = Some("/"),
          secure = false,
          httpOnly = true,
          otherAttributes = Map(
            "Version" -> Some("1"),
            "Something-Else" -> None
          )
        )
      )
    )
  }

  test("toString - should get string representation of cookie with all its attributes") {
    val cookieRawValue =
      "WELCOME_STATE_CHECKER=Xh3JYGEWrY7X2CIyI8_xqJ4UkipYtamLS2YOU27h8vY; Version=1; Something-Else; expires=Tue, 28-Jan-2020 17:13:10 GMT; max-Age=300; Path=/; HttpOnly"

    val result = CookieWithMeta.parse(cookieRawValue)

    result.right.get.toString shouldBe "WELCOME_STATE_CHECKER=Xh3JYGEWrY7X2CIyI8_xqJ4UkipYtamLS2YOU27h8vY; Expires=Tue, 28 Jan 2020 17:13:10 GMT; Max-Age=300; Path=/; HttpOnly; Version=1; Something-Else"
  }
}
