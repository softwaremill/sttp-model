package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.model.Encodings
import sttp.model.headers.AcceptEncoding.WeightedEncoding

class AcceptEncodingTest extends AnyFlatSpec with Matchers {

  it should "properly parse simplest Accept-Encoding header" in {
    AcceptEncoding.parse("deflate") shouldBe Right(AcceptEncoding(List(WeightedEncoding(Encodings.Deflate, None))))
  }

  it should "properly parse Accept-Encoding with wildcard" in {
    AcceptEncoding.parse("*") shouldBe Right(AcceptEncoding(List(WeightedEncoding(Encodings.Wildcard, None))))
  }

  it should "properly parse Accept-Encoding header with multiple encodings" in {
    val encodings = List(WeightedEncoding(Encodings.Gzip, None), WeightedEncoding(Encodings.Deflate, None))
    AcceptEncoding.parse("gzip, deflate") shouldBe Right(AcceptEncoding(encodings))
  }

  it should "properly parse Accept-Encoding header with multiple encodings and wildcard" in {
    val encodings =
      List(
        WeightedEncoding(Encodings.Gzip, None),
        WeightedEncoding(Encodings.Deflate, None),
        WeightedEncoding(Encodings.Wildcard, None)
      )
    AcceptEncoding.parse("gzip, deflate, *") shouldBe Right(AcceptEncoding(encodings))
  }

  it should "properly parse Accept-Encoding header with weight" in {
    val actual = AcceptEncoding.parse("deflate;q=1.0")
    actual shouldBe Right(AcceptEncoding(List(WeightedEncoding(Encodings.Deflate, Some(BigDecimal("1.0"))))))
  }

  it should "properly parse complex Accept-Encoding header" in {
    val encodings = List(
      WeightedEncoding(Encodings.Deflate, None),
      WeightedEncoding(Encodings.Gzip, Some(BigDecimal("0.75"))),
      WeightedEncoding(Encodings.Wildcard, Some(BigDecimal("0.5")))
    )
    AcceptEncoding.parse("deflate,gzip;q=0.75,*;q=0.5") shouldBe Right(AcceptEncoding(encodings))
  }

  it should "properly parse complex Accept-Encoding header with spaces" in {
    val encodings = List(
      WeightedEncoding(Encodings.Deflate, None),
      WeightedEncoding(Encodings.Gzip, Some(BigDecimal("0.75"))),
      WeightedEncoding(Encodings.Wildcard, Some(BigDecimal("0.5")))
    )
    AcceptEncoding.parse("deflate, gzip;q=0.75, *;q=0.5") shouldBe Right(AcceptEncoding(encodings))
  }

  it should "properly parse strange header" in {
    AcceptEncoding.parse("deflate,") shouldBe Right(AcceptEncoding(List(WeightedEncoding(Encodings.Deflate, None))))
  }

  it should "properly parse custom encoding" in {
    AcceptEncoding.parse("tapirus") shouldBe Right(AcceptEncoding(List(WeightedEncoding("tapirus", None))))
  }

  it should "properly parse custom encoding with weight" in {
    val encodings = List(WeightedEncoding("tapirus", Some(BigDecimal("0.1"))))
    AcceptEncoding.parse("tapirus;q=0.1") shouldBe Right(AcceptEncoding(encodings))
  }

  it should "fail while validating header with q more then 1" in {
    AcceptEncoding.parse("gzip;q=1.1") should matchPattern { case Left(_) => }
  }

  it should "fail while validating empty header" in {
    AcceptEncoding.parse("") should matchPattern { case Left(_) => }
  }

  it should "fail while parsing header with incorrect q" in {
    AcceptEncoding.parse("gzip;q1.1") should matchPattern { case Left(_) => }
  }

  it should "fail while parsing incorrect header" in {
    AcceptEncoding.parse(";") should matchPattern { case Left(_) => }
  }

  it should "fail while validating header without algorithm" in {
    AcceptEncoding.parse(";q=1.0") should matchPattern { case Left(_) => }
  }

  it should "display correct header string" in {
    val encodings = List(
      WeightedEncoding(Encodings.Deflate, None),
      WeightedEncoding(Encodings.Gzip, Some(BigDecimal("0.75"))),
      WeightedEncoding(Encodings.Wildcard, Some(BigDecimal("0.5")))
    )
    AcceptEncoding(encodings).toString shouldBe "deflate,gzip;q=0.75,*;q=0.5"
  }
}
