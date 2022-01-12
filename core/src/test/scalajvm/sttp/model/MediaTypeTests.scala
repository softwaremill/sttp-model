package sttp.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import sttp.model.ContentTypeRange.AnyRange

import scala.collection.immutable.Seq

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
    ("media type", "content type range", "matches"),
    (MediaType.ApplicationJson, AnyRange, true),
    (MediaType("*", "html"), ContentTypeRange("*", "json", "*"), true),
    (MediaType("text", "*"), ContentTypeRange("text", "*", "*"), true),
    (MediaType.ApplicationJson, ContentTypeRange("application", "*", "*"), true),
    (MediaType.ApplicationJson, ContentTypeRange("application", "json", "*"), true),
    //
    (MediaType.ApplicationJson.charset("utf-8"), ContentTypeRange("*", "*", "utf-16"), false),
    (MediaType("*", "html").charset("utf-8"), ContentTypeRange("*", "json", "utf-16"), false),
    (MediaType("text", "*").charset("utf-8"), ContentTypeRange("text", "*", "utf-16"), false),
    (MediaType.ApplicationJson.charset("utf-8"), ContentTypeRange("application", "*", "utf-16"), false),
    (MediaType.ApplicationJson.charset("utf-8"), ContentTypeRange("application", "json", "utf-16"), false),
    //
    (MediaType.ApplicationJson.charset("utf-8"), ContentTypeRange("application", "json", "*"), true)
  )

  forAll(matchCases) { (mt, range, matches) =>
    it should s"check match of $mt to $range" in {
      mt.matches(range) shouldBe matches
    }
  }

  private val bestMatchCases = Table(
    ("ranges", "best match"),
    (Seq(AnyRange), Some(MediaType.ApplicationJson.charset("utf-8"))),
    (Seq(ContentTypeRange("application", "json", "*")), Some(MediaType.ApplicationJson.charset("utf-8"))),
    (Seq(ContentTypeRange("application", "xml", "*")), Some(MediaType.ApplicationXml.charset("utf-8"))),
    (
      Seq(
        ContentTypeRange("application", "xml", "*"),
        ContentTypeRange("application", "json", "*")
      ),
      Some(MediaType.ApplicationXml.charset("utf-8"))
    ),
    (
      Seq(
        ContentTypeRange("application", "json", "*"),
        ContentTypeRange("application", "xml", "*")
      ),
      Some(MediaType.ApplicationJson.charset("utf-8"))
    ),
    (
      Seq(
        ContentTypeRange("application", "xml", "*"),
        ContentTypeRange("application", "json", "*"),
        ContentTypeRange("text", "html", "*")
      ),
      Some(MediaType.ApplicationXml.charset("utf-8"))
    ),
    (
      Seq(ContentTypeRange("text", "*", "*"), ContentTypeRange("application", "*", "*")),
      Some(MediaType.TextHtml.charset("utf-8"))
    ),
    (
      Seq(ContentTypeRange("*", "*", "iso-8859-1")),
      Some(MediaType.TextHtml.charset("iso-8859-1"))
    ),
    (
      Seq(ContentTypeRange("text", "html", "iso-8859-1"), ContentTypeRange("text", "html", "utf-8")),
      Some(MediaType.TextHtml.charset("iso-8859-1"))
    ),
    (
      Seq(ContentTypeRange("text", "csv", "*")),
      None
    ),
    (
      Seq(ContentTypeRange("text", "html", "utf-16")),
      None
    )
  )

  private val mediaTypesToBestMatch = Seq(
    MediaType.ApplicationJson.charset("utf-8"),
    MediaType.ApplicationXml.charset("utf-8"),
    MediaType.TextHtml.charset("utf-8"),
    MediaType.TextHtml.charset("iso-8859-1")
  )

  forAll(bestMatchCases) { (ranges, bestMatch) =>
    it should s"select $bestMatch for ranges $ranges" in {
      MediaType.bestMatch(mediaTypesToBestMatch, ranges) shouldBe bestMatch
    }
  }

  it should "be case-insensitive when comparing instances" in {
    MediaType.TextPlain.equals(MediaType.TextPlain) shouldBe true
    MediaType.TextPlain.equals(MediaType.TextPlain.copy(mainType = "TEXT")) shouldBe true
    MediaType.TextPlain.equals(MediaType.TextPlain.copy(subType = "PLAIN")) shouldBe true

    MediaType.TextPlain.equals(MediaType.TextPlainUtf8) shouldBe false
    MediaType.TextPlain.equals(MediaType.TextHtml) shouldBe false
  }
}
