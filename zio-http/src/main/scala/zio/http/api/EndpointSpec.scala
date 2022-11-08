package zio.http.api

import zio._
import zio.http.api.CodecType.Route
import zio.http.model.{Header, Headers, Method}
import zio.schema._
import zio.stream.ZStream
import zio.stacktracer.TracingImplicits.disableAutoTrace // scalafix:ok;

/**
 * An [[zio.http.api.EndpointSpec]] represents the specification for an endpoint
 * for the HTTP protocol. Every `EndpointSpec` has an input, which comes from a
 * combination of the HTTP path, query string parameters, headers, and body; and
 * an output, which is the data computed by the handler of the API and
 * translated into the body of the response.
 *
 * Because [[zio.http.api.EndpointSpec]] is a purely declarative encoding of an
 * endpoint, it is possible to use this model to generate a [[zio.http.HttpApp]]
 * (by supplying a handler for the endpoint), to generate OpenAPI documentation,
 * to generate a type-safe Scala client for the endpoint, and possibly, to
 * generate client libraries in other programming languages.
 */
final case class EndpointSpec[Input, Output](
  input: HttpCodec[CodecType.RequestType, Input],
  // error: HttpCodec[CodecType.ResponseType, Error],
  output: HttpCodec[CodecType.ResponseType, Output],
  doc: Doc,
) { self =>

  def apply(input: Input): Invocation[this.type, Input, Output] =
    Invocation(self, input)

  def apply[A, B](a: A, b: B)(implicit
    ev: (A, B) <:< Input,
  ): Invocation[this.type, Input, Output] =
    Invocation(self, ev((a, b)))

  def apply[A, B, C](a: A, b: B, c: C)(implicit
    ev: (A, B, C) <:< Input,
  ): Invocation[this.type, Input, Output] =
    Invocation(self, ev((a, b, c)))

  def apply[A, B, C, D](a: A, b: B, c: C, d: D)(implicit
    ev: (A, B, C, D) <:< Input,
  ): Invocation[this.type, Input, Output] =
    Invocation(self, ev((a, b, c, d)))

  def apply[A, B, C, D, E](a: A, b: B, c: C, d: D, e: E)(implicit
    ev: (A, B, C, D, E) <:< Input,
  ): Invocation[this.type, Input, Output] =
    Invocation(self, ev((a, b, c, d, e)))

  def apply[A, B, C, D, E, F](a: A, b: B, c: C, d: D, e: E, f: F)(implicit
    ev: (A, B, C, D, E, F) <:< Input,
  ): Invocation[this.type, Input, Output] =
    Invocation(self, ev((a, b, c, d, e, f)))

  def apply[A, B, C, D, E, F, G](a: A, b: B, c: C, d: D, e: E, f: F, g: G)(implicit
    ev: (A, B, C, D, E, F, G) <:< Input,
  ): Invocation[this.type, Input, Output] =
    Invocation(self, ev((a, b, c, d, e, f, g)))

  def apply[A, B, C, D, E, F, G, H](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H)(implicit
    ev: (A, B, C, D, E, F, G, H) <:< Input,
  ): Invocation[this.type, Input, Output] =
    Invocation(self, ev((a, b, c, d, e, f, g, h)))

  def apply[A, B, C, D, E, F, G, H, I](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I)(implicit
    ev: (A, B, C, D, E, F, G, H, I) <:< Input,
  ): Invocation[this.type, Input, Output] =
    Invocation(self, ev((a, b, c, d, e, f, g, h, i)))

  /**
   * Returns a new API that is derived from this one, but which includes
   * additional documentation that will be included in OpenAPI generation.
   */
  def ??(that: Doc): EndpointSpec[Input, Output] = copy(doc = self.doc + that)

  /**
   * Returns a new Endpoint specification with the specified header codec added
   * to the input of this specification.
   */
  def header[A](codec: HeaderCodec[A])(implicit
    combiner: Combiner[Input, A],
  ): EndpointSpec[combiner.Out, Output] =
    copy(input = self.input ++ codec)

  /**
   * Converts this endpoint specification into a concrete implementation of the
   * endpoint. In order to convert a spec into an implementation, you must
   * specify a function which handles the input, and which returns the output.
   */
  def implement[R, E](f: Input => ZIO[R, E, Output]): Endpoints[R, E, this.type] =
    Endpoints.HandledEndpoint[R, E, Input, Output, this.type](self, f)

  /**
   * Returns a new endpoint specification with the input type changed to the
   * specified type.
   */
  def in[Input2: Schema](implicit
    combiner: Combiner[Input, Input2],
  ): EndpointSpec[combiner.Out, Output] =
    copy(input = self.input ++ HttpCodec.Body(implicitly[Schema[Input2]]))

  /**
   * Returns a new endpoint specification with the input type changed to a
   * stream of the specified type.
   */
  def inStream[Input2: Schema](implicit
    combiner: Combiner[Input, ZStream[Any, Throwable, Input2]],
  ): EndpointSpec[combiner.Out, Output] =
    copy(input = self.input ++ HttpCodec.BodyStream(implicitly[Schema[Input2]]))

  /**
   * Changes the output type of the endpoint to the specified output type.
   */
  def out[Output2: Schema]: EndpointSpec[Input, Output2] =
    copy(output = HttpCodec.Body(implicitly[Schema[Output2]]))

  /**
   * Changes the output type of the endpoint to be a stream of the specified
   * output type.
   */
  def outStream[Output2: Schema]: EndpointSpec[Input, ZStream[Any, Throwable, Output2]] =
    copy(output = HttpCodec.BodyStream(implicitly[Schema[Output2]]))

  /**
   * Returns a new Endpoint specification with the specified query codec added
   * to the input of this specification.
   */
  def query[A](codec: QueryCodec[A])(implicit
    combiner: Combiner[Input, A],
  ): EndpointSpec[combiner.Out, Output] =
    copy(input = self.input ++ codec)

  /**
   * Returns a new Endpoint specification with the specified route codec added
   * to the input of this specification.
   */
  def route[A](codec: RouteCodec[A])(implicit
    combiner: Combiner[Input, A],
  ): EndpointSpec[combiner.Out, Output] =
    copy(input = self.input ++ codec)

  /**
   * Convert API to a ServiceSpec.
   */
  def toServiceSpec: ServiceSpec[Unit, Unit, this.type] =
    ServiceSpec(self).middleware(MiddlewareSpec.none)

  // def toOpenAPI: zio.http.api.openapi.??? = ???
}

object EndpointSpec {

  /**
   * Constructs an API for a DELETE endpoint, given the specified input. It is
   * not necessary to specify the full input to the endpoint upfront, as the
   * `API#in` method can be used to incrementally append additional input to the
   * definition of the API.
   */
  def delete[Input](route: RouteCodec[Input]): EndpointSpec[Input, Unit] = {
    EndpointSpec(route ++ MethodCodec.delete, HttpCodec.empty, Doc.empty)
  }

  /**
   * Constructs an API for a GET endpoint, given the specified input. It is not
   * necessary to specify the full input to the endpoint upfront, as the
   * `API#in` method can be used to incrementally append additional input to the
   * definition of the API.
   */
  def get[Input](route: RouteCodec[Input]): EndpointSpec[Input, Unit] =
    EndpointSpec(route ++ MethodCodec.get, HttpCodec.empty, Doc.empty)

  /**
   * Constructs an API for a POST endpoint, given the specified input. It is not
   * necessary to specify the full input to the endpoint upfront, as the
   * `API#in` method can be used to incrementally append additional input to the
   * definition of the API.
   */
  def post[Input](route: RouteCodec[Input]): EndpointSpec[Input, Unit] =
    EndpointSpec(route ++ MethodCodec.post, HttpCodec.empty, Doc.empty)

  /**
   * Constructs an API for a PUT endpoint, given the specified input. It is not
   * necessary to specify the full input to the endpoint upfront, as the
   * `API#in` method can be used to incrementally append additional input to the
   * definition of the API.
   */
  def put[Input](route: RouteCodec[Input]): EndpointSpec[Input, Unit] =
    EndpointSpec(route ++ MethodCodec.put, HttpCodec.empty, Doc.empty)
}
