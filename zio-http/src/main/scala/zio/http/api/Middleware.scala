package zio.http.api

import io.netty.handler.codec.http.HttpHeaderNames
import zio._
import zio.http._
import zio.http.api.MiddlewareSpec.{CsrfValidate, decodeHttpBasic}
import zio.http.middleware.Auth
import zio.http.middleware.Cors.CorsConfig
import zio.http.model.Headers.{BasicSchemeName, BearerSchemeName, Header}
import zio.http.model.headers.values.Origin
import zio.http.model.{Cookie, Headers, HttpError, Method, Status}

import java.util.{Base64, UUID}

/**
 * A `Middleware` represents the implementation of a `MiddlewareSpec`,
 * intercepting parts of the request, and appending to the response.
 */
sealed trait Middleware[-R] { self =>
  type Error 
  type Input
  type Output
  type State

  /**
   * Processes an incoming request, whose relevant parts are encoded into
   * `Input`, the middleware input, and then returns an effect that will produce
   * both middleware-specific state (which will be passed to the outgoing
   * handlerr), together with a decision about whether to continue or abort the
   * handling of the request.
   */
  def incoming(in: Input): ZIO[R, Option[Error], State]

  /**
   * Processes an outgoing response together with the middleware state (produced
   * by the incoming handler), returning an effect that will produce `Output`,
   * which will in turn be used to patch the response.
   */
  def outgoing(state: State): ZIO[R, Nothing, Output]

  /**
   * Applies the middleware to an `HttpApp`, returning a new `HttpApp` with the
   * middleware fully installed.
   */
  def apply[R1 <: R, E](httpApp: HttpApp[R1, E]): HttpApp[R1, E] =
    Http.fromOptionFunction[Request] { request =>
      for {
        in       <- spec.input.decodeRequest(request).orDie
        response <- incoming(in).foldZIO({
          case None => ZIO.fail(None)
          case Some(error) => 
            val patch    = spec.error.encodeResponsePatch(error)
            val response = patch(Response.ok)

            ZIO.succeed(response)
        },
          state => 
            for {
              response1 <- httpApp(request)
              mo        <- outgoing(state)
              patch = spec.output.encodeResponsePatch(mo)
            } yield response1.patch(patch)
        )
      } yield response
    }

  def ++[R1 <: R](that: Middleware[R1])(implicit
    inCombiner: Combiner[self.Input, that.Input],
    outCombiner: Combiner[self.Output, that.Output],
    errCombiner: Alternator[self.Error, that.Error]
  ): Middleware[R1] =
    Middleware.Concat[R1, self.Error, that.Error, errCombiner.Out, Input, Output, that.Input, that.Output, inCombiner.Out, outCombiner.Out](
      self,
      that,
      inCombiner,
      outCombiner,
      errCombiner
    )

  def spec: MiddlewareSpec[Input, Error, Output]
}

object Middleware {
  def intercept[S, R, Input, Output](spec: MiddlewareSpec[Input, _, Output])(incoming: Input => S)(
    outgoing: S => Output,
  ): Middleware[R] =
    interceptZIO(spec)(i => ZIO.succeedNow(incoming(i)))(s => ZIO.succeedNow(outgoing(s)))

  def interceptZIO[S]: Interceptor1[S] = new Interceptor1[S]

  /**
   * Sets cookie in response headers
   */
  def addCookie(cookie: Cookie[Response]): Middleware[Any] =
    fromFunction(MiddlewareSpec.addCookie)(_ => cookie)

  def addCookieZIO[R](cookie: ZIO[R, Nothing, Cookie[Response]]): Middleware[R] =
    fromFunctionZIO(MiddlewareSpec.addCookie)(_ => cookie)

  /**
   * Creates a middleware for basic authentication
   */
  final def basicAuth(f: Auth.Credentials => Boolean)(implicit trace: Trace): Middleware[Any] =
    basicAuthZIO(credentials => ZIO.succeed(f(credentials)))

  /**
   * Creates a middleware for basic authentication that checks if the
   * credentials are same as the ones given
   */
  final def basicAuth(u: String, p: String)(implicit trace: Trace): Middleware[Any] =
    basicAuth { credentials => (credentials.uname == u) && (credentials.upassword == p) }

  /**
   * Creates a middleware for basic authentication using an effectful
   * verification function
   */
  def basicAuthZIO[R](f: Auth.Credentials => ZIO[R, Nothing, Boolean])(implicit
    trace: Trace,
  ): Middleware[R] =
    customAuthZIO(HeaderCodec.authorization, HeaderCodec.wwwAuthenticate) { encoded =>
      val indexOfBasic = encoded.indexOf(BasicSchemeName)
      if (indexOfBasic != 0 || encoded.length == BasicSchemeName.length) ZIO.succeed(false)
      else {
        // TODO: probably should be part of decodeHttpBasic
        val readyForDecode = new String(Base64.getDecoder.decode(encoded.substring(BasicSchemeName.length + 1)))
        decodeHttpBasic(readyForDecode) match {
          case Some(credentials) => f(credentials)
          case None              => ZIO.succeed(false)
        }
      }
    }

  /**
   * Creates a middleware for bearer authentication that checks the token using
   * the given function
   *
   * @param f
   *   : function that validates the token string inside the Bearer Header
   */
  final def bearerAuth(f: String => Boolean)(implicit trace: Trace): Middleware[Any] =
    bearerAuthZIO(token => ZIO.succeed(f(token)))

  /**
   * Creates a middleware for bearer authentication that checks the token using
   * the given effectful function
   *
   * @param f
   *   : function that effectfully validates the token string inside the Bearer
   *   Header
   */
  final def bearerAuthZIO[R](
    f: String => ZIO[R, Nothing, Boolean],
  )(implicit trace: Trace): Middleware[R] =
    customAuthZIO(
      HeaderCodec.authorization,
      responseHeaders = Headers(HttpHeaderNames.WWW_AUTHENTICATE, BearerSchemeName),
    ) { token =>
      val indexOfBearer = token.indexOf(BearerSchemeName)
      if (indexOfBearer != 0 || token.length == BearerSchemeName.length)
        ZIO.succeed(false)
      else
        f(token.substring(BearerSchemeName.length + 1))
    }

  /**
   * Creates an authentication middleware that only allows authenticated
   * requests to be passed on to the app.
   */
  def customAuth[R, Input](headerCodec: HeaderCodec[Input])(
    verify: Input => Boolean,
  ): Middleware[R] =
    customAuthZIO(headerCodec)(header => ZIO.succeed(verify(header)))

  /**
   * Creates an authentication middleware that only allows authenticated
   * requests to be passed on to the app using an effectful verification
   * function.
   */
  def customAuthZIO[R, E, Input](
    headerCodec: HeaderCodec[Input],
    unauthorized: HttpCodec[CodecType.ResponseType, E]
  )(verify: Input => ZIO[R, E, Unit])(implicit trace: Trace): Middleware[R] =
    MiddlewareSpec.customAuth(headerCodec, unauthorized).implementIncomingControl { in =>
      verify(in)
    }

  /**
   * Creates a middleware for Cross-Origin Resource Sharing (CORS).
   *
   * @see
   *   https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS
   */
  def cors(config: CorsConfig = CorsConfig()) = {
    def allowCORS(origin: Origin, method: Method): Boolean =
      (config.anyOrigin, config.anyMethod, Origin.fromOrigin(origin), method) match {
        case (true, true, _, _)           => true
        case (true, false, _, acrm)       => config.allowedMethods.exists(_.contains(acrm))
        case (false, true, origin, _)     => config.allowedOrigins(origin)
        case (false, false, origin, acrm) =>
          config.allowedMethods.exists(_.contains(acrm)) && config.allowedOrigins(origin)
      }

    def corsHeaders(origin: Origin, isPreflight: Boolean): Headers = {
      def buildHeaders(headerName: String, values: Option[Set[String]]): Headers =
        values match {
          case Some(headerValues) =>
            Headers(headerValues.toList.map(value => Header(headerName, value)))
          case None               => Headers.empty
        }

      Headers.ifThenElse(isPreflight)(
        onTrue = buildHeaders(HttpHeaderNames.ACCESS_CONTROL_ALLOW_HEADERS.toString(), config.allowedHeaders),
        onFalse = buildHeaders(HttpHeaderNames.ACCESS_CONTROL_EXPOSE_HEADERS.toString(), config.exposedHeaders),
      ) ++
        Headers(HttpHeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN.toString(), Origin.fromOrigin(origin)) ++
        buildHeaders(
          HttpHeaderNames.ACCESS_CONTROL_ALLOW_METHODS.toString(),
          config.allowedMethods.map(_.map(_.toJava.name())),
        ) ++
        Headers.when(config.allowCredentials) {
          Headers(HttpHeaderNames.ACCESS_CONTROL_ALLOW_CREDENTIALS, config.allowCredentials.toString)
        }
    }

    MiddlewareSpec.cors.implement {
      case (Method.OPTIONS, Some(origin), Some(acrm)) if allowCORS(origin, Method.fromString(acrm)) =>
        ZIO
          .succeed(
            Middleware.Control.Abort(
              Response.Patch(setStatus = Some(Status.NoContent), addHeaders = corsHeaders(origin, isPreflight = true)),
            ),
          )

      case (method, Some(origin), _) if allowCORS(origin, method) =>
        ZIO
          .succeed(
            Middleware.Control
              .Abort(Response.Patch(setStatus = None, addHeaders = corsHeaders(origin, isPreflight = false))),
          )

      case _ => ZIO.unit
    } { case (_, _) =>
      ZIO.unit
    }
  }

  /**
   * Generates a new CSRF token that can be validated using the csrfValidate
   * middleware.
   *
   * CSRF middlewares: To prevent Cross-site request forgery attacks. This
   * middleware is modeled after the double submit cookie pattern. Used in
   * conjunction with [[#csrfValidate]] middleware.
   *
   * https://cheatsheetseries.owasp.org/cheatsheets/Cross-Site_Request_Forgery_Prevention_Cheat_Sheet.html#double-submit-cookie
   */
  final def csrfGenerate[R, E](
    tokenName: String = "x-csrf-token",
    tokenGen: ZIO[R, Nothing, String] = ZIO.succeed(UUID.randomUUID.toString)(Trace.empty),
  )(implicit trace: Trace): api.Middleware[R] = {
    api.Middleware.addCookieZIO(tokenGen.map(Cookie(tokenName, _)))
  }

  /**
   * Validates the CSRF token appearing in the request headers. Typically the
   * token should be set using the `csrfGenerate` middleware.
   *
   * CSRF middlewares : To prevent Cross-site request forgery attacks. This
   * middleware is modeled after the double submit cookie pattern. Used in
   * conjunction with [[#csrfGenerate]] middleware
   *
   * https://cheatsheetseries.owasp.org/cheatsheets/Cross-Site_Request_Forgery_Prevention_Cheat_Sheet.html#double-submit-cookie
   */
  def csrfValidate(tokenName: String = "x-csrf-token"): Middleware[Any] =
    MiddlewareSpec
      .csrfValidate(tokenName)
      .implement {
        case state @ CsrfValidate(Some(cookieValue), Some(tokenValue)) if cookieValue.content == tokenValue =>
          ZIO.succeedNow(Control.Continue(state))

        case state =>
          ZIO.succeedNow(Control.Abort(Response.Patch(setStatus = Some(Status.Forbidden), addHeaders = Headers.empty)))
      }((_, _) => ZIO.unit)

  def fromFunction[A, B](spec: MiddlewareSpec[A, _, B])(
    f: A => B,
  ): Middleware[Any] =
    intercept(spec)((a: A) => Control.Continue(a))((a, _) => f(a))

  def fromFunctionZIO[R, E, A, B](spec: MiddlewareSpec[A, E, B])(
    f: A => ZIO[R, E, B],
  ): Middleware[R] =
    interceptZIO(spec)(f)(ZIO.succeedNow(_))

  val none: Middleware[Any] =
    fromFunction(MiddlewareSpec.none)(_ => ())

  class Interceptor1[S](val dummy: Boolean = true) extends AnyVal {
    def apply[R, E, Input, Output](spec: MiddlewareSpec[Input, E, Output])(
      incoming: Input => ZIO[R, E, S],
    ): Interceptor2[S, R, E, Input, Output] =
      new Interceptor2[S, R, E, Input, Output](spec, incoming)
  }

  class Interceptor2[S, R, E, Input, Output](
    spec: MiddlewareSpec[Input, E, Output],
    incoming: Input => ZIO[R, E, Control[S]],
  ) {
    def apply[R1 <: R](outgoing: S => ZIO[R1, Nothing, Output]): Middleware[R1] =
      InterceptZIO(spec, incoming, outgoing)
  }

  private[api] final case class InterceptZIO[S, R, E, Input0, Output0](
    spec: MiddlewareSpec[Input0, E, Output0],
    incoming0: Input0 => ZIO[R, E, Control[S]],
    outgoing0: S => ZIO[R, Nothing, Output0],
  ) extends Middleware[R] {
    type Error = E
    type Input  = Input0
    type Output = Output0
    type State  = S

    def incoming(in: Input): ZIO[R, Option[E], Middleware.Control[State]] = incoming0(in).mapError(Some(_))

    def outgoing(state: State): ZIO[R, Nothing, Output] = outgoing0(state)
  }
  private[api] final case class Concat[-R, E1, E2, E3, I1, O1, I2, O2, I3, O3](
    left: Middleware[R] { type Input = I1; type Output = O1; type Error = E1 },
    right: Middleware[R] { type Input = I2; type Output = O2; type Error = E2 },
    inCombiner: Combiner.WithOut[I1, I2, I3],
    outCombiner: Combiner.WithOut[O1, O2, O3],
    errCombiner: Alternator.WithOut[E1, E2, E3]
  ) extends Middleware[R] {
    type Error  = E3 
    type Input  = I3
    type Output = O3
    type State  = (left.State, right.State)

    def incoming(in: I3): ZIO[R, Option[Error], State] = {
      val (l, r) = inCombiner.separate(in)

      for {
        leftState  <- left.incoming(l).mapError(_.flatMap(errCombiner.left(_)))
        rightState <- right.incoming(r).mapError(_.map(errCombiner.right(_)))
      } yield (leftState, rightState)
    }

    def outgoing(state: State): ZIO[R, Nothing, O3] =
      for {
        l <- left.outgoing(state._1)
        r <- right.outgoing(state._2)
      } yield outCombiner.combine(l, r)

    def spec: MiddlewareSpec[Input, Error, Output] =
      left.spec.++(right.spec)(inCombiner, outCombiner, errCombiner)
  }
}
