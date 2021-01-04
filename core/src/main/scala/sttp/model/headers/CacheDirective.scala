package sttp.model.headers

import sttp.model.internal.Validate.RichEither

import scala.concurrent.duration.{DurationInt, FiniteDuration}

sealed trait CacheDirective

object CacheDirective {
  case class MaxAge(d: FiniteDuration) extends CacheDirective { override def toString = s"max-age=${d.toSeconds}" }
  case class MaxStale(d: Option[FiniteDuration]) extends CacheDirective {
    override def toString = s"max-stale${d.fold("")(_.toSeconds.toString)}"
  }
  case class MinFresh(d: FiniteDuration) extends CacheDirective { override def toString = s"min-fresh=${d.toSeconds}" }
  case object NoCache extends CacheDirective { override def toString = "no-cache" }
  case object NoStore extends CacheDirective { override def toString = "no-store" }
  case object NoTransform extends CacheDirective { override def toString = "no-transform" }
  case object OnlyIfCached extends CacheDirective { override def toString = "only-if-cached" }
  case object MustRevalidate extends CacheDirective { override def toString = "must-revalidate" }
  case object Public extends CacheDirective { override def toString = "public" }
  case object Private extends CacheDirective { override def toString = "private" }
  case object ProxyRevalidate extends CacheDirective { override def toString = "proxy-revalidate" }
  case class SMaxage(d: FiniteDuration) extends CacheDirective { override def toString = s"s-maxage=${d.toSeconds}" }
  case object Immutable extends CacheDirective { override def toString = "immutable" }
  case class StaleWhileRevalidate(d: FiniteDuration) extends CacheDirective {
    override def toString = s"stale-while-revalidate=${d.toSeconds}"
  }
  case class StaleIfError(d: FiniteDuration) extends CacheDirective {
    override def toString = s"stale-if-error=${d.toSeconds}"
  }

  private val MaxAgePattern = "max-age=(\\d+)".r
  private val MaxStalePattern = "max-stale(=\\d+)?".r
  private val MinFreshPattern = "min-fresh=(\\d+)".r
  private val SMaxagePattern = "s-maxage=(\\d+)".r
  private val StaleWhileRevalidatePattern = "stale-while-revalidate=(\\d+)".r
  private val StaleIfErrorPattern = "stale-if-error=(\\d+)".r

  def parse(s: String): List[Either[String, CacheDirective]] = {
    s.split(",").map(_.trim.toLowerCase).toList.map {
      case MaxAgePattern(c)               => Right(MaxAge(c.toInt.seconds))
      case MaxStalePattern(c)             => Right(MaxStale(Option(c).map(_.substring(1).toInt.seconds)))
      case MinFreshPattern(c)             => Right(MinFresh(c.toInt.seconds))
      case "no-cache"                     => Right(NoCache)
      case "no-store"                     => Right(NoStore)
      case "no-transform"                 => Right(NoTransform)
      case "only-if-cached"               => Right(OnlyIfCached)
      case "must-revalidate"              => Right(MustRevalidate)
      case "public"                       => Right(Public)
      case "private"                      => Right(Private)
      case "proxy-revalidate"             => Right(ProxyRevalidate)
      case SMaxagePattern(c)              => Right(SMaxage(c.toInt.seconds))
      case "immutable"                    => Right(Immutable)
      case StaleWhileRevalidatePattern(c) => Right(StaleWhileRevalidate(c.toInt.seconds))
      case StaleIfErrorPattern(c)         => Right(StaleIfError(c.toInt.seconds))
      case v                              => Left(s"Unknown cache directive: $v")
    }
  }
  def unsafeParse(s: String): List[CacheDirective] = parse(s).map(_.getOrThrow)
}
