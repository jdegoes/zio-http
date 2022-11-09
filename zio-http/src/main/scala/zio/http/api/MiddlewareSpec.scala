package zio.http.api

import zio.ZIO
import zio.http.api.internal.TextCodec
import zio.http.middleware.Auth
import zio.http.middleware.Auth.Credentials
import zio.http.model.Headers.BasicSchemeName
import zio.http.model.{Cookie, HTTP_CHARSET, HeaderNames}
import zio.http.{Request, Response}

import java.util.Base64

final case class MiddlewareSpec[MiddlewareIn, MiddlewareErr, MiddlewareOut](
  input: HttpCodec[CodecType.Header with CodecType.Query with CodecType.Method, MiddlewareIn],
  output: HttpCodec[CodecType.Header, MiddlewareOut],
  error: HttpCodec[CodecType.ResponseType, MiddlewareErr]
) { self =>
  def ++[MiddlewareIn2, MiddlewareErr2, MiddlewareOut2](that: MiddlewareSpec[MiddlewareIn2, MiddlewareErr2, MiddlewareOut2])(implicit
    inCombiner: Combiner[MiddlewareIn, MiddlewareIn2],
    outCombiner: Combiner[MiddlewareOut, MiddlewareOut2],
    errAlternator: Alternator[MiddlewareErr, MiddlewareErr2]
  ): MiddlewareSpec[inCombiner.Out, errAlternator.Out, outCombiner.Out] =
    MiddlewareSpec(self.input ++ that.input, self.output ++ that.output, ???)

  def implement[R, S](
    incoming: MiddlewareIn => ZIO[R, MiddlewareErr, S],
  ): Middleware.Interceptor2[S, R, MiddlewareErr, MiddlewareIn, MiddlewareOut] =
    Middleware.interceptZIO(self)(incoming)

  def implementIncoming[R](
    incoming: MiddlewareIn => ZIO[R, Nothing, MiddlewareOut],
  ): Middleware[R] =
    Middleware.fromFunctionZIO(self)(incoming)

  def implementIncomingControl[R](
    incoming: MiddlewareIn => ZIO[R, MiddlewareErr, MiddlewareOut],
  ): Middleware[R] =
    implement[R, MiddlewareOut](in => incoming(in))(ZIO.succeedNow(_))

  def mapIn[MiddlewareIn2](
    f: HttpCodec[CodecType.Header with CodecType.Query with CodecType.Method, MiddlewareIn] => HttpCodec[
      CodecType.Header with CodecType.Query with CodecType.Method,
      MiddlewareIn2,
    ],
  ): MiddlewareSpec[MiddlewareIn2, MiddlewareErr, MiddlewareOut] =
    copy(input = f(input))

  def mapOut[MiddlewareOut2](
    f: HttpCodec[CodecType.Header, MiddlewareOut] => HttpCodec[
      CodecType.Header,
      MiddlewareOut2,
    ],
  ): MiddlewareSpec[MiddlewareIn, MiddlewareErr, MiddlewareOut2] =
    copy(output = f(output))

  def mapBoth[MiddlewareIn2, MiddlewareOut2](
    f: HttpCodec[CodecType.Header with CodecType.Query with CodecType.Method, MiddlewareIn] => HttpCodec[
      CodecType.Header with CodecType.Query with CodecType.Method,
      MiddlewareIn2,
    ],
    g: HttpCodec[CodecType.Header, MiddlewareOut] => HttpCodec[
      CodecType.Header,
      MiddlewareOut2,
    ],
  ): MiddlewareSpec[MiddlewareIn2, MiddlewareErr, MiddlewareOut2] =
    mapIn(f).mapOut(g)

  def optional: MiddlewareSpec[Option[MiddlewareIn], MiddlewareErr, Option[MiddlewareOut]] =
    self.optionalIn.optionalOut

  def optionalIn: MiddlewareSpec[Option[MiddlewareIn], MiddlewareErr, MiddlewareOut] =
    self.mapIn(_.optional)

  def optionalOut: MiddlewareSpec[MiddlewareIn, MiddlewareErr, Option[MiddlewareOut]] =
    self.mapOut(_.optional)

}

object MiddlewareSpec {

  final case class CsrfValidate(cookieOption: Option[Cookie[Request]], tokenValue: Option[String])

  def none: MiddlewareSpec[Unit, Unit, Unit] =
    MiddlewareSpec(HttpCodec.empty, HttpCodec.empty, HttpCodec.empty)

  def cookieOption(cookieName: String): MiddlewareSpec[Option[Cookie[Request]], Unit, Unit] =
    requireHeader(HeaderNames.cookie.toString()).optionalIn
      .mapIn(
        _.transformOrFail(
          {
            case Some(cookieList) => readCookie(cookieList, cookieName)
            case None             => Right(None)
          },
          {
            case Some(cookie) => writeCookie(cookie).map(Some(_))
            case None         => Right(None)
          },
        ),
      )

  def cookie(cookieName: String): MiddlewareSpec[Cookie[Request], Unit, Unit] = {
    cookieOption(cookieName).mapIn(
      _.transformOrFailLeft(
        {
          case Some(cookie) => Right(cookie)
          case None         => Left(s"Cookie ${cookieName} not found")
        },
        value => Some(value),
      ),
    )
  }

  def csrfValidate(tokenName: String): MiddlewareSpec[CsrfValidate, Unit, Unit] = {
    val cookie: MiddlewareSpec[Option[Cookie[Request]], Unit, Unit] =
      MiddlewareSpec.cookieOption(tokenName)

    val tokenHeader =
      MiddlewareSpec.requireHeader(tokenName)

    (cookie ++ tokenHeader.mapIn(_.optional)).mapIn(
      _.transform(
        { case (a, b) =>
          CsrfValidate(a, b)
        },
        value => (value.cookieOption, value.tokenValue),
      ),
    )
  }

  def addCookie: MiddlewareSpec[Unit, Unit, Cookie[Response]] =
    MiddlewareSpec(
      HttpCodec.empty,
      HeaderCodec.cookie.transformOrFail(
        str => Cookie.decode[Response](str).left.map(_.getMessage),
        _.encode(validate = false).left.map(_.getMessage),
      ),
      HttpCodec.empty
    )

  /**
   * Add specified header to the response
   */
  def addHeader(key: String, value: String): MiddlewareSpec[Unit, Unit, Unit] =
    MiddlewareSpec(HttpCodec.empty, HeaderCodec.header(key, TextCodec.constant(value)), HttpCodec.empty)

  def addCorrelationId: MiddlewareSpec[Unit, Unit, String] =
    MiddlewareSpec(HttpCodec.empty, HeaderCodec.header("-x-correlation-id", TextCodec.string), HttpCodec.empty)

  def withAuthorization(value: CharSequence): MiddlewareSpec[Unit, Unit, Unit] =
    addHeader(HeaderNames.authorization.toString, value.toString)

  def withBasicAuthorization(username: String, password: String): MiddlewareSpec[Unit, Unit, Unit] = {
    val authString    = String.format("%s:%s", username, password)
    val encodedAuthCB = new String(Base64.getEncoder.encode(authString.getBytes(HTTP_CHARSET)), HTTP_CHARSET)
    val value         = String.format("%s %s", BasicSchemeName, encodedAuthCB)
    addHeader(HeaderNames.authorization.toString, value)
  }

  def auth: MiddlewareSpec[Auth.Credentials, Unit, Unit] =
    requireHeader(HeaderNames.wwwAuthenticate.toString)
      .mapIn(
        _.transformOrFailLeft(
          s => decodeHttpBasic(s).fold(Left("Failed to decode headers"): Either[String, Credentials])(Right(_)),
          c => s"${c.uname}:${c.upassword}",
        ),
      )

  def cors =
    MiddlewareSpec(
      input = MethodCodec.method ++
        HeaderCodec.origin.optional ++
        HeaderCodec.accessControlRequestMethod.optional,
      output = HttpCodec.empty,
      error = HttpCodec.empty 
    )

  def customAuth[I, E](
    headerCodec: HeaderCodec[I],
    unauthorized: HttpCodec[CodecType.ResponseType, E]
  ): MiddlewareSpec[I, E, Unit] =
    MiddlewareSpec(
      headerCodec,
      HttpCodec.empty,
      unauthorized
    )

  def requireHeader(name: String): MiddlewareSpec[String, Unit, Unit] =
    MiddlewareSpec(HeaderCodec.header(name, TextCodec.string), HttpCodec.empty, HttpCodec.empty)

  private[api] def decodeHttpBasic(encoded: String): Option[Credentials] = {
    val colonIndex = encoded.indexOf(":")
    if (colonIndex == -1)
      None
    else {
      val username = encoded.substring(0, colonIndex)
      val password =
        if (colonIndex == encoded.length - 1)
          ""
        else
          encoded.substring(colonIndex + 1)
      Some(Credentials(username, password))
    }
  }

  private[api] def readCookie(
    cookieList: String,
    cookieName: String,
  ): Either[String, Option[Cookie[Request]]] =
    Cookie
      .decode[Request](cookieList)
      .map(list => list.find(_.name == cookieName))
      .left
      .map(_.getMessage)

  private[api] def writeCookie(cookieRequest: Cookie[Request]): Either[String, String] =
    cookieRequest
      .encode(validate = false)
      .left
      .map(_.getMessage)

}
