package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.model.Encodings

class AcceptEncodingTest extends AnyFlatSpec with Matchers {

  it should "properly parse simplest AcceptEncoding header" in {
    val actual = AcceptEncoding.parse("deflate").head
    actual shouldBe Right(AcceptEncoding(Encodings.Deflate, None))
  }

  it should "properly parse AcceptEncoding with wildcard" in {
    val actual = AcceptEncoding.parse("*").head
    actual shouldBe Right(AcceptEncoding(Encodings.Wildcard, None))
  }

  it should "properly parse AcceptEncoding header with multiple encodings" in {
    val actual = AcceptEncoding.parse("gzip, deflate")
    val gzip = Right(AcceptEncoding(Encodings.Gzip, None))
    val deflate = Right(AcceptEncoding(Encodings.Deflate, None))
    actual shouldBe List(gzip, deflate)
  }

  it should "properly parse AcceptEncoding header with multiple encodings and wildcard" in {
    val actual = AcceptEncoding.parse("gzip, deflate, *")
    val gzip = Right(AcceptEncoding(Encodings.Gzip, None))
    val deflate = Right(AcceptEncoding(Encodings.Deflate, None))
    val wildcard= Right(AcceptEncoding(Encodings.Wildcard, None))
    actual shouldBe List(gzip, deflate, wildcard)
  }

  it should "properly parse AcceptEncoding header with weight" in {
    val actual = AcceptEncoding.parse("deflate;q=1.0").head
    actual shouldBe Right(AcceptEncoding(Encodings.Deflate, Some(BigDecimal("1.0"))))
  }

  it should "properly parse complex AcceptEncoding header" in {
    val actual = AcceptEncoding.parse("deflate, gzip;q=0.75, *;q=0.5")
    val deflate = Right(AcceptEncoding(Encodings.Deflate, None))
    val gzip = Right(AcceptEncoding(Encodings.Gzip, Some(BigDecimal("0.75"))))
    val wildcard= Right(AcceptEncoding(Encodings.Wildcard, Some(BigDecimal("0.5"))))
    actual shouldBe List(deflate, gzip, wildcard)
  }

  it should "properly parse strange header" in {
    val actual = AcceptEncoding.parse("deflate,")
    actual shouldBe List(Right(AcceptEncoding(Encodings.Deflate, None)))
  }

  it should "properly parse custom encoding" in {
    val actual = AcceptEncoding.parse("tapirus").head
    actual shouldBe Right(AcceptEncoding("tapirus", None))
  }

  it should "properly parse custom encoding with weight" in {
    val actual = AcceptEncoding.parse("tapirus;q=0.1").head
    actual shouldBe Right(AcceptEncoding("tapirus", Some(BigDecimal("0.1"))))
  }

  it should "fail while validating header with q more then 1" in {
    val actual = AcceptEncoding.parse("gzip;q=1.1").head
    actual shouldBe Left("Invalid Encoding")
  }

  it should "fail while validating empty header" in {
    val actual = AcceptEncoding.parse("").head
    actual shouldBe Left("Invalid Encoding")
  }

  it should "fail while parsing header with incorrect q" in {
    val actual = AcceptEncoding.parse("gzip;q1.1").head
    actual shouldBe Left("Expected accept-encoding weight in the format: \"q=1.0\", but got: q1.1")
  }

  it should "fail while parsing incorrect header" in {
    val actual = AcceptEncoding.parse(";").head
    actual shouldBe Left("Expected accept-encoding in the format: \"deflate or gzip;q=1.0\", but got: ;")
  }

  it should "fail while validating header without algorithm" in {
    val actual = AcceptEncoding.parse(";q=1.0").head
    actual shouldBe Left("Invalid Encoding")
  }
}
