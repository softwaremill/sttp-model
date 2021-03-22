package sttp.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class MediaTypeTests extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {
  val parseMediaTypeData = List(
    "text/html" -> Right(MediaType.unsafeApply("text", "html")),
    "text/html; charset=UTF-8" -> Right(MediaType.unsafeApply("text", "html", Some("UTF-8"))),
    "text/plain;a=1;b=2;charset=utf-8;c=3" -> Right(
      MediaType.unsafeApply("text", "plain", Some("utf-8"), Map("a" -> "1", "b" -> "2", "c" -> "3"))
    ),
    "text/html;" -> Right(MediaType.unsafeApply("text", "html")),
    "multipart/form-data" -> Right(MediaType.unsafeApply("multipart", "form-data")),
    "application/atom+xml" -> Right(MediaType.unsafeApply("application", "atom+xml")),
    "te<t/plain" -> Left("""No subtype found for: "te<t/plain""""),
    "text/plain; a=1; b=" -> Left("""Parameter is not formatted correctly: "b=" for: "text/plain; a=1; b=""""),
    "text/plain;a=1;b=2;charset=utf-8;b=5" -> Left(
      """Multiple "b" defined: "2" and: "5" for: "text/plain;a=1;b=2;charset=utf-8;b=5""""
    ),
    "text/plain;a=1;b=2;charset=utf-8;charset=iso-8859-1" -> Left(
      """Multiple "charset" defined: "utf-8" and: "iso-8859-1" for: "text/plain;a=1;b=2;charset=utf-8;charset=iso-8859-1""""
    )
  )

  for ((mediaTypeValue, expectedResult) <- parseMediaTypeData) {
    it should s"parse or error: $mediaTypeValue" in {
      MediaType.parse(mediaTypeValue) shouldBe expectedResult
    }
  }

  val serializeMediaTypeData = List(
    MediaType.unsafeApply("text", "html") -> "text/html",
    MediaType.unsafeApply("text", "html", Some("utf-8")) -> "text/html; charset=utf-8",
    MediaType.unsafeApply("multipart", "form-data") -> "multipart/form-data",
    MediaType.unsafeApply("application", "atom+xml") -> "application/atom+xml",
    MediaType.unsafeApply("text", "html", Some("utf-8"), Map("a" -> "1")) -> "text/html; charset=utf-8; a=1"
  )

  for ((mediaType, expectedResult) <- serializeMediaTypeData) {
    it should s"serialize $mediaType to $expectedResult" in {
      mediaType.toString shouldBe expectedResult
    }
  }

  it should "validate media types" in {
    MediaType.safeApply("text", "p=lain") should matchPattern { case Left(_) => }
    MediaType.safeApply("text", "plain", Some("UTF=8")) should matchPattern { case Left(_) => }
    MediaType.safeApply("text", "plain") shouldBe Right(MediaType.TextPlain)
    MediaType.safeApply("text", "plain", None, Map("a" -> "{1}")) should matchPattern { case Left(_) => }
    MediaType.safeApply("text", "plain", None, Map("a" -> "/1/")) should matchPattern { case Left(_) => }
  }

  it should "throw exceptions on invalid media types" in {
    an[IllegalArgumentException] shouldBe thrownBy(MediaType.unsafeApply("te=xt", "plain"))
  }

  private val matchCases = Table(
    ("media type", "other media type", "matches"),
    (MediaType.ApplicationJson, MediaType("*", "*"), true),
    (MediaType("*", "html"), MediaType("*", "json"), true),
    (MediaType("text", "*"), MediaType("text", "*"), true),
    (MediaType.ApplicationJson, MediaType("application", "*"), true),
    (MediaType.ApplicationJson, MediaType.ApplicationJson, true),
    //
    (MediaType.ApplicationJson.charset("utf-8"), MediaType("*", "*").charset("utf-16"), false),
    (MediaType("*", "html").charset("utf-8"), MediaType("*", "json").charset("utf-16"), false),
    (MediaType("text", "*").charset("utf-8"), MediaType("text", "*").charset("utf-16"), false),
    (MediaType.ApplicationJson.charset("utf-8"), MediaType("application", "*").charset("utf-16"), false),
    (MediaType.ApplicationJson.charset("utf-8"), MediaType.ApplicationJson.charset("utf-16"), false),
    //
    (MediaType.ApplicationJson.charset("utf-8"), MediaType.ApplicationJson.charset("*"), true)
  )

  forAll(matchCases) { (mt1, mt2, matches) =>
    it should s"compare $mt1 with $mt2" in {
      mt1.matches(mt2) shouldBe matches
    }
  }

}
