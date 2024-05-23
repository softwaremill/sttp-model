package sttp.model

import org.scalatest.TryValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sttp.model.Uri._

class UriTestsHelper extends AnyFunSuite with Matchers with TryValues {

  val QS = QuerySegment

  val testUri = Uri.unsafeApply("http", None, "example.com", None, Nil, Nil, None)

  val querySegmentsTestData = List(
    List(QS.KeyValue("k", "v1~v2", valueEncoding = QuerySegmentEncoding.All)) -> "k=v1~v2"
  )

  for {
    (segments, expected) <- querySegmentsTestData
  } {
    test(s"$segments should serialize to$expected") {
      testUri.copy(querySegments = segments).toString should endWith(expected)
    }
  }
}
