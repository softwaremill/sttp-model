package sttp.model

import java.net.URI
import sttp.model.Uri.QuerySegment.{KeyValue, Plain, Value}
import sttp.model.Uri.{
  Authority,
  Encoding,
  FragmentSegment,
  HostSegment,
  PathSegment,
  PathSegments,
  QuerySegment,
  Segment,
  UserInfo
}
import sttp.model.internal.{Rfc3986, UriCompatibility, Validate}
import sttp.model.internal.Validate._

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.util.{Failure, Success, Try}
import sttp.model.internal.Rfc3986.encode

import scala.collection.mutable

/** A [[https://en.wikipedia.org/wiki/Uniform_Resource_Identifier URI]]. Can represent both relative and absolute URIs,
  * hence in terms of [[https://tools.ietf.org/html/rfc3986]], this is a URI reference.
  *
  * All components (scheme, host, query, ...) are stored decoded, and become encoded upon serialization (using
  * [[toString]]).
  *
  * Instances can be created using the uri interpolator: `uri"..."` (see [[UriInterpolator]]), or the factory methods on
  * the [[Uri]] companion object.
  *
  * The `apply`/`safeApply`/`unsafeApply` methods create absolute URIs and require a host. The `relative` methods
  * creates a relative URI, given path/query/fragment components.
  *
  * @param querySegments
  *   Either key-value pairs, single values, or plain query segments. Key value pairs will be serialized as `k=v`, and
  *   blocks of key-value pairs/single values will be combined using `&`. Note that no `&` or other separators are added
  *   around plain query segments - if required, they need to be added manually as part of the plain query segment.
  *   Custom encoding logic can be provided when creating a segment.
  */
case class Uri(
    scheme: Option[String],
    authority: Option[Authority],
    pathSegments: PathSegments,
    querySegments: Seq[QuerySegment],
    fragmentSegment: Option[Segment]
) {

  /** Replace the scheme. Does not validate the new scheme value. */
  def scheme(s: String): Uri = this.copy(scheme = Some(s))

  /** Replace the scheme. Does not validate the new scheme value. */
  def scheme(s: Option[String]): Uri = this.copy(scheme = s)

  //

  /** Replace the user info with a username only. Adds an empty host if one is absent. */
  def userInfo(username: String): Uri = userInfo(Some(UserInfo(username, None)))

  /** Replace the user info with username/password combination. Adds an empty host if one is absent. */
  def userInfo(username: String, password: String): Uri = userInfo(Some(UserInfo(username, Some(password))))

  /** Replace the user info with username/password combination. Adds an empty host if one is absent, and user info is
    * defined.
    */
  def userInfo(ui: Option[UserInfo]): Uri = ui match {
    case Some(v) => this.copy(authority = Some(authority.getOrElse(Authority.Empty).userInfo(Some(v))))
    case None    => this.copy(authority = authority.map(_.userInfo(None)))
  }

  def userInfo: Option[UserInfo] = authority.flatMap(_.userInfo)

  /** Replace the host. Does not validate the new host value if it's nonempty. */
  def host(h: String): Uri = hostSegment(HostSegment(h))

  /** Replace the host. Does not validate the new host value if it's nonempty. */
  def hostSegment(s: Segment): Uri = hostSegment(Some(s))

  /** Replace the host. Does not validate the new host value if it's nonempty. */
  def hostSegment(s: Option[Segment]): Uri = this.copy(authority = authority match {
    case Some(a) => s.map(a.hostSegment(_))
    case None    => s.map(Authority(None, _, None))
  })

  def host: Option[String] = authority.map(_.hostSegment.v)

  /** Replace the port. Adds an empty host if one is absent. */
  def port(p: Int): Uri = port(Some(p))

  /** Replace the port. Adds an empty host if one is absent, and port is defined. */
  def port(p: Option[Int]): Uri = p match {
    case Some(v) => this.copy(authority = Some(authority.getOrElse(Authority.Empty).port(v)))
    case None    => this.copy(authority = authority.map(_.port(None)))
  }

  def port: Option[Int] = authority.flatMap(_.port)

  /** Replace the authority. */
  def authority(a: Authority): Uri = this.copy(authority = Some(a))

  /** Replace the authority. */
  def authority(a: Some[Authority]): Uri = this.copy(authority = a)

  //

  /** Replace path with the given single-segment path. */
  @deprecated(message = "Use addPath, withPath or withWholePath", since = "1.2.0")
  def path(p: String): Uri = withWholePath(p)

  /** Replace path with the given path segments. */
  @deprecated(message = "Use addPath, withPath or withWholePath", since = "1.2.0")
  def path(p1: String, p2: String, ps: String*): Uri = withPath(p1 :: p2 :: ps.toList)

  /** Replace path with the given path segments. */
  @deprecated(message = "Use addPath, withPath or withWholePath", since = "1.2.0")
  def path(ps: scala.collection.Seq[String]): Uri = withPath(ps)

  /** Replace path with the given path segment. */
  @deprecated(message = "Use addPath, withPath or withWholePath", since = "1.2.0")
  def pathSegment(s: Segment): Uri = withPathSegment(s)

  /** Replace path with the given path segment. */
  @deprecated(message = "Use addPath, withPath or withWholePath", since = "1.2.0")
  def pathSegments(s1: Segment, s2: Segment, ss: Segment*): Uri = withPathSegments(s1, s2, ss: _*)

  /** Replace path with the given path segments. */
  @deprecated(message = "Use addPath, withPath or withWholePath", since = "1.2.0")
  def pathSegments(ss: scala.collection.Seq[Segment]): Uri = withPathSegments(ss.toList)

  def addPath(p: String, ps: String*): Uri = addPath(p :: ps.toList)
  def addPath(ps: scala.collection.Seq[String]): Uri = addPathSegments(ps.toList.map(PathSegment(_)))
  def addPathSegment(s: Segment): Uri = addPathSegments(List(s))
  def addPathSegments(s1: Segment, s2: Segment, ss: Segment*): Uri = addPathSegments(s1 :: s2 :: ss.toList)
  def addPathSegments(ss: scala.collection.Seq[Segment]): Uri = copy(pathSegments = pathSegments.addSegments(ss))

  def withPath(p: String, ps: String*): Uri = withPath(p :: ps.toList)
  def withPath(ps: scala.collection.Seq[String]): Uri = withPathSegments(ps.toList.map(PathSegment(_)))
  def withPathSegment(s: Segment): Uri = withPathSegments(List(s))
  def withPathSegments(s1: Segment, s2: Segment, ss: Segment*): Uri = withPathSegments(s1 :: s2 :: ss.toList)
  def withPathSegments(ss: scala.collection.Seq[Segment]): Uri = copy(pathSegments = pathSegments.withSegments(ss))

  /** Replace the whole path with the given one. Leading `/` will be removed, if present, and the path will be split
    * into segments on `/`.
    */
  def withWholePath(p: String): Uri = {
    // removing the leading slash, as it is added during serialization anyway
    val pWithoutLeadingSlash = if (p.startsWith("/")) p.substring(1) else p
    val ps = pWithoutLeadingSlash.split("/", -1).toList
    withPath(ps)
  }

  def path: Seq[String] = pathSegments.segments.map(_.v).toList

  //

  /** Adds the given parameter to the query. */
  @deprecated(message = "Use addParam or withParam", since = "1.2.0")
  def param(k: String, v: String): Uri = addParam(k, v)

  /** Adds the given parameter with an optional value to the query if it is present. */
  @deprecated(message = "Use addParam or withParam", since = "1.2.0")
  def param(k: String, v: Option[String]): Uri = addParam(k, v)

  /** Adds the given parameters to the query. */
  @deprecated(message = "Use addParam or withParam", since = "1.2.0")
  def params(ps: Map[String, String]): Uri = addParams(ps)

  /** Adds the given parameters to the query. */
  @deprecated(message = "Use addParam or withParam", since = "1.2.0")
  def params(mqp: QueryParams): Uri = addParams(mqp)

  /** Adds the given parameters to the query. */
  @deprecated(message = "Use addParam or withParam", since = "1.2.0")
  def params(ps: (String, String)*): Uri = addParams(ps: _*)

  def addParam(k: String, v: String): Uri = addParams(k -> v)
  def addParam(k: String, v: Option[String]): Uri = v.map(addParam(k, _)).getOrElse(this)
  def addParams(ps: Map[String, String]): Uri = addParams(ps.toSeq: _*)
  def addParams(mqp: QueryParams): Uri = {
    this.copy(querySegments = querySegments ++ QuerySegment.fromQueryParams(mqp))
  }
  def addParams(ps: (String, String)*): Uri = {
    this.copy(querySegments = querySegments ++ ps.map { case (k, v) =>
      KeyValue(k, v)
    })
  }

  /** Replace query with the given single parameter. */
  def withParam(k: String, v: String): Uri = withParams(k -> v)

  /** Replace query with the given single optional parameter. */
  def withParam(k: String, v: Option[String]): Uri = v.map(withParam(k, _)).getOrElse(this)

  /** Replace query with the given parameters. */
  def withParams(ps: Map[String, String]): Uri = withParams(ps.toSeq: _*)

  /** Replace query with the given parameters. */
  def withParams(mqp: QueryParams): Uri = this.copy(querySegments = QuerySegment.fromQueryParams(mqp).toList)

  /** Replace query with the given parameters. */
  def withParams(ps: (String, String)*): Uri = this.copy(querySegments = ps.map { case (k, v) =>
    KeyValue(k, v)
  }.toList)

  def paramsMap: Map[String, String] = params.toMap

  def params: QueryParams = {
    val m = new mutable.LinkedHashMap[String, List[String]] // keeping parameter order
    querySegments.foreach {
      case KeyValue(k, v, _, _) => m.update(k, m.getOrElse(k, Nil) ++ List(v))
      case Value(v, _)          => m.update(v, m.getOrElse(v, Nil))
      case Plain(v, _)          => m.update(v, m.getOrElse(v, Nil))
    }
    QueryParams.fromMultiSeq(m.toSeq)
  }

  def paramsSeq: Seq[(String, String)] = params.toSeq.toList

  /** Adds the given query segment. */
  @deprecated(message = "Use addQuerySegment", since = "1.2.0")
  def querySegment(qf: QuerySegment): Uri = addQuerySegment(qf)

  def addQuerySegment(qf: QuerySegment): Uri = this.copy(querySegments = querySegments :+ qf)

  //

  /** Replace the fragment. */
  def fragment(f: String): Uri = fragment(Some(f))

  /** Replace the fragment. */
  def fragment(f: Option[String]): Uri = fragmentSegment(f.map(FragmentSegment(_)))

  /** Replace the fragment. */
  def fragmentSegment(s: Option[Segment]): Uri = this.copy(fragmentSegment = s)

  def fragment: Option[String] = fragmentSegment.map(_.v)

  //

  def toJavaUri: URI = new URI(toString())

  def isAbsolute: Boolean = scheme.isDefined
  def isRelative: Boolean = !isAbsolute

  def resolve(other: Uri): Uri = Uri(toJavaUri.resolve(other.toJavaUri))

  //

  def hostSegmentEncoding(encoding: Encoding): Uri =
    copy(authority = authority.map(a => a.copy(hostSegment = a.hostSegment.encoding(encoding))))

  def pathSegmentsEncoding(encoding: Encoding): Uri = copy(pathSegments = pathSegments match {
    case Uri.EmptyPath              => Uri.EmptyPath
    case Uri.AbsolutePath(segments) => Uri.AbsolutePath(segments.map(_.encoding(encoding)))
    case Uri.RelativePath(segments) => Uri.RelativePath(segments.map(_.encoding(encoding)))
  })

  /** Replace encoding for query segments: applies to key-value, only-value and plain ones. */
  def querySegmentsEncoding(encoding: Encoding): Uri = copy(querySegments = querySegments.map {
    case KeyValue(k, v, _, _) => KeyValue(k, v, encoding, encoding)
    case Value(v, _)          => Value(v, encoding)
    case Plain(v, _)          => Plain(v, encoding)
  })

  /** Replace encoding for the value part of key-value query segments and for only-value ones. */
  def queryValueSegmentsEncoding(valueEncoding: Encoding): Uri = copy(querySegments = querySegments.map {
    case KeyValue(k, v, keyEncoding, _) => KeyValue(k, v, keyEncoding, valueEncoding)
    case Value(v, _)                    => Value(v, valueEncoding)
    case s                              => s
  })

  def fragmentSegmentEncoding(encoding: Encoding): Uri =
    copy(fragmentSegment = fragmentSegment.map(f => f.encoding(encoding)))

  override def toString: String = {
    @tailrec
    def encodeQuerySegments(qss: List[QuerySegment], previousWasPlain: Boolean, sb: StringBuilder): String =
      qss match {
        case Nil => sb.toString()

        case Plain(v, enc) :: t =>
          encodeQuerySegments(t, previousWasPlain = true, sb.append(enc(v)))

        case Value(v, enc) :: t =>
          if (!previousWasPlain) sb.append("&")
          sb.append(enc(v))
          encodeQuerySegments(t, previousWasPlain = false, sb)

        case KeyValue(k, v, kEnc, vEnc) :: t =>
          if (!previousWasPlain) sb.append("&")
          sb.append(kEnc(k)).append("=").append(vEnc(v))
          encodeQuerySegments(t, previousWasPlain = false, sb)
      }

    val schemeS = scheme.map(s => encode(Rfc3986.Scheme)(s) + ":").getOrElse("")
    val authorityS = authority.fold("")(_.toString)
    val pathPrefixS = pathSegments match {
      case _ if authority.isEmpty && scheme.isDefined => ""
      case Uri.EmptyPath                              => ""
      case Uri.AbsolutePath(_)                        => "/"
      case Uri.RelativePath(_)                        => ""
    }
    val pathS = pathSegments.segments.map(_.encoded).mkString("/")
    val queryPrefixS = if (querySegments.isEmpty) "" else "?"

    val queryS = encodeQuerySegments(querySegments.toList, previousWasPlain = true, new StringBuilder())

    // https://stackoverflow.com/questions/2053132/is-a-colon-safe-for-friendly-url-use/2053640#2053640
    val fragS = fragmentSegment.fold("")(s => "#" + s.encoded)

    s"$schemeS$authorityS$pathPrefixS$pathS$queryPrefixS$queryS$fragS"
  }
}

/** For a general description of the behavior of `apply`, `parse`, `safeApply` and `unsafeApply` methods, see
  * [[sttp.model]].
  *
  * The `safeApply` methods return a validation error if the scheme contains illegal characters or if the host is empty.
  */
object Uri extends UriInterpolator {
  private val AllowedSchemeCharacters = "[a-zA-Z][a-zA-Z0-9+-.]*".r
  private def validateHost(host: Option[String]): Option[String] =
    host.flatMap(h => if (h.isEmpty) Some("Host cannot be empty") else None)
  private def validateScheme(scheme: Option[String]) = scheme.flatMap { s =>
    if (AllowedSchemeCharacters.unapplySeq(s).isEmpty)
      Some("Scheme can only contain alphanumeric characters, +, - and .")
    else None
  }

  def safeApply(host: String): Either[String, Uri] =
    safeApply("http", Some(Authority(host)), Vector.empty, Vector.empty, None)
  def safeApply(host: String, port: Int): Either[String, Uri] =
    safeApply("http", Some(Authority(host, port)), Vector.empty, Vector.empty, None)
  def safeApply(host: String, port: Int, path: Seq[String]): Either[String, Uri] =
    safeApply("http", Some(Authority(host, port)), path.map(PathSegment(_)), Vector.empty, None)
  def safeApply(scheme: String, path: Seq[String]): Either[String, Uri] =
    safeApply(scheme, None, path.map(PathSegment(_)), Vector.empty, None)
  def safeApply(scheme: String, host: String): Either[String, Uri] =
    safeApply(scheme, Some(Authority(host)), Vector.empty, Vector.empty, None)
  def safeApply(scheme: String, host: String, port: Int): Either[String, Uri] =
    safeApply(scheme, Some(Authority(host, port)), Vector.empty, Vector.empty, None)
  def safeApply(scheme: String, host: String, port: Int, path: Seq[String]): Either[String, Uri] =
    safeApply(scheme, Some(Authority(host, port)), path.map(PathSegment(_)), Vector.empty, None)
  def safeApply(scheme: String, host: String, path: Seq[String]): Either[String, Uri] =
    safeApply(scheme, Some(Authority(host)), path.map(PathSegment(_)), Vector.empty, None)
  def safeApply(scheme: String, host: String, path: Seq[String], fragment: Option[String]): Either[String, Uri] =
    safeApply(
      scheme,
      Some(Authority(host)),
      path.map(PathSegment(_)),
      Vector.empty,
      fragment.map(FragmentSegment(_))
    )
  def safeApply(
      scheme: String,
      userInfo: Option[UserInfo],
      host: String,
      port: Option[Int],
      path: Seq[String],
      querySegments: Seq[QuerySegment],
      fragment: Option[String]
  ): Either[String, Uri] =
    safeApply(
      scheme,
      Some(Authority(userInfo, HostSegment(host), port)),
      path.map(PathSegment(_)),
      querySegments,
      fragment.map(FragmentSegment(_))
    )
  def safeApply(
      scheme: String,
      authority: Option[Authority],
      pathSegments: Seq[Segment],
      querySegments: Seq[QuerySegment],
      fragmentSegment: Option[Segment]
  ): Either[String, Uri] =
    Validate.all(validateScheme(Some(scheme)), validateHost(authority.map(_.hostSegment.v)))(
      apply(
        Some(scheme),
        authority,
        PathSegments.absoluteOrEmpty(pathSegments),
        querySegments,
        fragmentSegment
      )
    )

  //

  def unsafeApply(host: String): Uri =
    unsafeApply("http", Some(Authority(host)), Vector.empty, Vector.empty, None)
  def unsafeApply(host: String, port: Int): Uri =
    unsafeApply("http", Some(Authority(host, port)), Vector.empty, Vector.empty, None)
  def unsafeApply(host: String, port: Int, path: Seq[String]): Uri =
    unsafeApply("http", Some(Authority(host, port)), path.map(PathSegment(_)), Vector.empty, None)
  def unsafeApply(scheme: String, path: Seq[String]): Uri =
    unsafeApply(scheme, None, path.map(PathSegment(_)), Vector.empty, None)
  def unsafeApply(scheme: String, host: String): Uri =
    unsafeApply(scheme, Some(Authority(host)), Vector.empty, Vector.empty, None)
  def unsafeApply(scheme: String, host: String, port: Int): Uri =
    unsafeApply(scheme, Some(Authority(host, port)), Vector.empty, Vector.empty, None)
  def unsafeApply(scheme: String, host: String, port: Int, path: Seq[String]): Uri =
    unsafeApply(scheme, Some(Authority(host, port)), path.map(PathSegment(_)), Vector.empty, None)
  def unsafeApply(scheme: String, host: String, path: Seq[String]): Uri =
    unsafeApply(scheme, Some(Authority(host)), path.map(PathSegment(_)), Vector.empty, None)
  def unsafeApply(scheme: String, host: String, path: Seq[String], fragment: Option[String]): Uri =
    unsafeApply(
      scheme,
      Some(Authority(host)),
      path.map(PathSegment(_)),
      Vector.empty,
      fragment.map(FragmentSegment(_))
    )
  def unsafeApply(
      scheme: String,
      userInfo: Option[UserInfo],
      host: String,
      port: Option[Int],
      path: Seq[String],
      querySegments: Seq[QuerySegment],
      fragment: Option[String]
  ): Uri =
    unsafeApply(
      scheme,
      Some(Authority(userInfo, HostSegment(host), port)),
      path.map(PathSegment(_)),
      querySegments,
      fragment.map(FragmentSegment(_))
    )
  def unsafeApply(
      scheme: String,
      authority: Option[Authority],
      pathSegments: Seq[Segment],
      querySegments: Seq[QuerySegment],
      fragmentSegment: Option[Segment]
  ): Uri =
    safeApply(scheme, authority, pathSegments, querySegments, fragmentSegment).getOrThrow

  //

  def apply(host: String): Uri =
    apply(Some("http"), Some(Authority(host)), EmptyPath, Vector.empty, None)
  def apply(host: String, port: Int): Uri =
    apply(Some("http"), Some(Authority(host, port)), EmptyPath, Vector.empty, None)
  def apply(host: String, port: Int, path: Seq[String]): Uri =
    apply(Some("http"), Some(Authority(host, port)), PathSegments.absoluteOrEmptyS(path), Vector.empty, None)
  def apply(scheme: String, path: Seq[String]): Uri =
    apply(Some(scheme), None, PathSegments.absoluteOrEmptyS(path), Vector.empty, None)
  def apply(scheme: String, host: String): Uri =
    apply(Some(scheme), Some(Authority(host)), EmptyPath, Vector.empty, None)
  def apply(scheme: String, host: String, port: Int): Uri =
    apply(Some(scheme), Some(Authority(host, port)), EmptyPath, Vector.empty, None)
  def apply(scheme: String, host: String, port: Int, path: Seq[String]): Uri =
    apply(Some(scheme), Some(Authority(host, port)), PathSegments.absoluteOrEmptyS(path), Vector.empty, None)
  def apply(scheme: String, host: String, path: Seq[String]): Uri =
    apply(Some(scheme), Some(Authority(host)), PathSegments.absoluteOrEmptyS(path), Vector.empty, None)
  def apply(scheme: String, host: String, path: Seq[String], fragment: Option[String]): Uri =
    apply(
      Some(scheme),
      Some(Authority(host)),
      PathSegments.absoluteOrEmptyS(path),
      Vector.empty,
      fragment.map(FragmentSegment(_))
    )
  def apply(
      scheme: String,
      userInfo: Option[UserInfo],
      host: String,
      port: Option[Int],
      path: Seq[String],
      querySegments: Seq[QuerySegment],
      fragment: Option[String]
  ): Uri = {
    apply(
      Some(scheme),
      Some(Authority(userInfo, HostSegment(host), port)),
      PathSegments.absoluteOrEmptyS(path),
      querySegments,
      fragment.map(FragmentSegment(_))
    )
  }
  def apply(
      scheme: String,
      authority: Option[Authority],
      path: Seq[Segment],
      querySegments: Seq[QuerySegment],
      fragment: Option[Segment]
  ): Uri = {
    apply(
      Some(scheme),
      authority,
      PathSegments.absoluteOrEmpty(path),
      querySegments,
      fragment
    )
  }

  //

  /** Create a relative URI with an absolute path. */
  def relative(path: Seq[String]): Uri = relative(path, Vector.empty, None)

  /** Create a relative URI with an absolute path. */
  def relative(path: Seq[String], fragment: Option[String]): Uri = relative(path, Vector.empty, fragment)

  /** Create a relative URI with an absolute path. */
  def relative(path: Seq[String], querySegments: Seq[QuerySegment], fragment: Option[String]): Uri =
    apply(None, None, PathSegments.absoluteOrEmptyS(path), querySegments, fragment.map(FragmentSegment(_)))

  /** Create a relative URI with a relative path. */
  def pathRelative(path: Seq[String]): Uri = pathRelative(path, Vector.empty, None)

  /** Create a relative URI with a relative path. */
  def pathRelative(path: Seq[String], fragment: Option[String]): Uri = pathRelative(path, Vector.empty, fragment)

  /** Create a relative URI with a relative path. */
  def pathRelative(path: Seq[String], querySegments: Seq[QuerySegment], fragment: Option[String]): Uri =
    apply(None, None, RelativePath(path.map(PathSegment(_))), querySegments, fragment.map(FragmentSegment(_)))

  //

  def apply(javaUri: URI): Uri = uri"${javaUri.toString}"

  def parse(uri: String): Either[String, Uri] =
    Try(uri"$uri") match {
      case Success(u)            => Right(u)
      case Failure(e: Exception) => Left(e.getMessage)
      case Failure(t: Throwable) => throw t
    }

  def unsafeParse(uri: String): Uri = uri"$uri"

  //

  case class Authority(userInfo: Option[UserInfo], hostSegment: Segment, port: Option[Int]) {

    /** Replace the user info with a username only. */
    def userInfo(username: String): Authority = this.copy(userInfo = Some(UserInfo(username, None)))

    /** Replace the user info with username/password combination. */
    def userInfo(username: String, password: String): Authority =
      this.copy(userInfo = Some(UserInfo(username, Some(password))))

    /** Replace the user info. */
    def userInfo(ui: Option[UserInfo]): Authority = this.copy(userInfo = ui)

    /** Replace the host. Does not validate the new host value if it's nonempty. */
    def host(h: String): Authority = hostSegment(HostSegment(h))

    /** Replace the host. Does not validate the new host value if it's nonempty. */
    def hostSegment(s: Segment): Authority = this.copy(hostSegment = s)

    def host: String = hostSegment.v

    /** Replace the port. */
    def port(p: Int): Authority = port(Some(p))

    /** Replace the port. */
    def port(p: Option[Int]): Authority = this.copy(port = p)

    override def toString: String = {
      def encodeUserInfo(ui: UserInfo): String =
        encode(Rfc3986.UserInfo)(ui.username) + ui.password.fold("")(":" + encode(Rfc3986.UserInfo)(_))

      val userInfoS = userInfo.fold("")(encodeUserInfo(_) + "@")
      val hostS = hostSegment.encoded
      val portS = port.fold("")(":" + _)

      s"//$userInfoS$hostS$portS"
    }
  }
  object Authority {
    private[model] val Empty = Authority("")

    def safeApply(host: String): Either[String, Authority] =
      Validate.all(validateHost(Some(host)))(Authority(None, HostSegment(host), None))
    def safeApply(host: String, port: Int): Either[String, Authority] =
      Validate.all(validateHost(Some(host)))(Authority(None, HostSegment(host), Some(port)))
    def unsafeApply(host: String): Authority = safeApply(host).getOrThrow
    def unsafeApply(host: String, port: Int): Authority = safeApply(host, port).getOrThrow
    def apply(host: String): Authority = Authority(None, HostSegment(host), None)
    def apply(host: String, port: Int): Authority = Authority(None, HostSegment(host), Some(port))
  }

  //

  case class Segment(v: String, encoding: Encoding) {
    def encoded: String = encoding(v)
    def encoding(e: Encoding): Segment = copy(encoding = e)
  }

  object HostSegment {
    def apply(v: String): Segment = Segment(v, HostEncoding.Standard)
  }

  sealed trait PathSegments {
    def segments: collection.Seq[Segment]

    def add(p: String, ps: String*): PathSegments = add(p :: ps.toList)
    def add(ps: scala.collection.Seq[String]): PathSegments = addSegments(ps.toList.map(PathSegment(_)))
    def addSegment(s: Segment): PathSegments = addSegments(List(s))
    def addSegments(s1: Segment, s2: Segment, ss: Segment*): PathSegments = addSegments(s1 :: s2 :: ss.toList)
    def addSegments(ss: scala.collection.Seq[Segment]): PathSegments = {
      val base = if (segments.lastOption.exists(_.v.isEmpty)) segments.init else segments
      withSegments(base ++ ss.toList)
    }

    def withS(p: String, ps: String*): PathSegments = withS(p :: ps.toList)
    def withS(ps: scala.collection.Seq[String]): PathSegments = withSegments(ps.toList.map(PathSegment(_)))
    def withSegment(s: Segment): PathSegments = withSegments(List(s))
    def withSegments(s1: Segment, s2: Segment, ss: Segment*): PathSegments = withSegments(s1 :: s2 :: ss.toList)
    def withSegments(ss: scala.collection.Seq[Segment]): PathSegments
  }
  object PathSegments {
    def absoluteOrEmptyS(segments: Seq[String]): PathSegments = absoluteOrEmpty(segments.map(PathSegment(_)))
    def absoluteOrEmpty(segments: Seq[Segment]): PathSegments =
      if (segments.isEmpty) EmptyPath else AbsolutePath(segments)
  }
  case object EmptyPath extends PathSegments {
    override def withSegments(ss: collection.Seq[Segment]): PathSegments = AbsolutePath(ss.toList)
    override def segments: collection.Seq[Segment] = Nil
    override def toString: String = ""
  }
  case class AbsolutePath(segments: Seq[Segment]) extends PathSegments {
    override def withSegments(ss: scala.collection.Seq[Segment]): AbsolutePath = copy(segments = ss.toList)
    override def toString: String = "/" + segments.map(_.encoded).mkString("/")
  }
  case class RelativePath(segments: Seq[Segment]) extends PathSegments {
    override def withSegments(ss: scala.collection.Seq[Segment]): RelativePath = copy(segments = ss.toList)
    override def toString: String = segments.map(_.encoded).mkString("/")
  }

  object PathSegment {
    def apply(v: String): Segment = Segment(v, PathSegmentEncoding.Standard)
  }

  object FragmentSegment {
    def apply(v: String): Segment = Segment(v, FragmentEncoding.Standard)
  }

  sealed trait QuerySegment
  object QuerySegment {

    /** @param keyEncoding
      *   See [[Plain.encoding]]
      * @param valueEncoding
      *   See [[Plain.encoding]]
      */
    case class KeyValue(
        k: String,
        v: String,
        keyEncoding: Encoding = QuerySegmentEncoding.Standard,
        valueEncoding: Encoding = QuerySegmentEncoding.Standard
    ) extends QuerySegment {
      override def toString = s"KeyValue($k,$v,[keyEncoding],[valueEncoding])"
    }

    /** A query fragment which contains only the value, without a key. */
    case class Value(v: String, encoding: Encoding = QuerySegmentEncoding.StandardValue) extends QuerySegment {
      override def toString = s"Value($v,[encoding])"
    }

    /** A query fragment which will be inserted into the query, without and preceding or following separators. Allows
      * constructing query strings which are not (only) &-separated key-value pairs.
      *
      * @param encoding
      *   How to encode the value, and which characters should be escaped. The RFC3986 standard defines that the query
      *   can include these special characters, without escaping:
      * {{{
      * /?:@-._~!$&()*+,;=
      * }}}
      * See: [[https://stackoverflow.com/questions/2322764/what-characters-must-be-escaped-in-an-http-query-string]]
      * [[https://stackoverflow.com/questions/2366260/whats-valid-and-whats-not-in-a-uri-query]]
      */
    case class Plain(v: String, encoding: Encoding = QuerySegmentEncoding.StandardValue) extends QuerySegment {
      override def toString = s"Plain($v,[encoding])"
    }

    private[model] def fromQueryParams(mqp: QueryParams): Iterable[QuerySegment] = {
      mqp.toMultiSeq.flatMap { case (k, vs) =>
        vs match {
          case Seq() => List(Value(k))
          case s     => s.map(v => KeyValue(k, v))
        }
      }
    }
  }

  type Encoding = String => String

  object HostEncoding {
    private val IpV6Pattern = "[0-9a-fA-F:]+".r

    val Standard: Encoding = {
      case s @ IpV6Pattern() if s.count(_ == ':') >= 2 => s"[$s]"
      case s                                           => UriCompatibility.encodeDNSHost(s)
    }
  }

  object PathSegmentEncoding {
    val Standard: Encoding = encode(Rfc3986.PathSegment)
  }

  object QuerySegmentEncoding {

    /** Encodes all reserved characters using [[java.net.URLEncoder.encode()]]. */
    val All: Encoding = UriCompatibility.encodeQuery(_, "UTF-8")

    /** Encodes only the `&` and `=` reserved characters, which are usually used to separate query parameter names and
      * values.
      */
    val Standard: Encoding = encode(Rfc3986.Query -- Set('&', '='), spaceAsPlus = true, encodePlus = true)

    /** Encodes only the `&` reserved character, which is usually used to separate query parameter names and values. The
      * '=' sign is allowed in values.
      */
    val StandardValue: Encoding = encode(Rfc3986.Query -- Set('&'), spaceAsPlus = true, encodePlus = true)

    /** Doesn't encode any of the reserved characters, leaving intact all characters allowed in the query string as
      * defined by RFC3986.
      */
    val Relaxed: Encoding = encode(Rfc3986.Query, spaceAsPlus = true)

    /** Doesn't encode any of the reserved characters, leaving intact all characters allowed in the query string as
      * defined by RFC3986 as well as the characters `[` and `]`. These brackets aren't legal in the query part of the
      * URI, but some servers use them unencoded. See
      * https://stackoverflow.com/questions/11490326/is-array-syntax-using-square-brackets-in-url-query-strings-valid
      * for discussion.
      */
    val RelaxedWithBrackets: Encoding = encode(Rfc3986.QueryWithBrackets, spaceAsPlus = true)
  }

  object FragmentEncoding {
    val Standard: Encoding = encode(Rfc3986.Fragment)
  }

  case class UserInfo(username: String, password: Option[String])
}
