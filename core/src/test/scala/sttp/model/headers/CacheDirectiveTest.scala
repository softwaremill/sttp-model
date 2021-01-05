package sttp.model.headers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.DurationInt

class CacheDirectiveTest extends AnyFlatSpec with Matchers {
  it should "parse cache directives" in {
    CacheDirective.parse("no-cache") shouldBe List(Right(CacheDirective.NoCache))
    CacheDirective.parse("max-age=10") shouldBe List(Right(CacheDirective.MaxAge(10.seconds)))
    CacheDirective.parse("public, NO-STORE, stale-while-revalidate=2") shouldBe List(
      Right(CacheDirective.Public),
      Right(CacheDirective.NoStore),
      Right(CacheDirective.StaleWhileRevalidate(2.seconds))
    )
    CacheDirective.parse("max-stale, max-stale=10") shouldBe List(
      Right(CacheDirective.MaxStale(None)),
      Right(CacheDirective.MaxStale(Some(10.seconds)))
    )
    CacheDirective.parse("x") should matchPattern { case List(Left(_)) => }
    CacheDirective.parse("no-cache, x, public") should matchPattern {
      case List(Right(CacheDirective.NoCache), Left(_), Right(CacheDirective.Public)) =>
    }
  }
}
