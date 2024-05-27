package sttp.model

import org.scalatest.TryValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sttp.model.Uri._

trait UriTestsExtension extends AnyFunSuite with Matchers with TryValues { this: UriTests =>

  private val tildeEncodingTest = List(
    List(QS.KeyValue("k", "v1~v2", valueEncoding = QuerySegmentEncoding.All)) -> "k=v1%7Ev2"
  )

  for {
    (segments, expected) <- tildeEncodingTest
  } {
    test(s"$segments should serialize to$expected") {
      testUri.copy(querySegments = segments).toString should endWith(expected)
    }
  }
}
