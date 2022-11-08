package zio.http.api

import zio._
import zio.http.middleware.Auth
import zio.http.{Client, Request, Server, URL}
import zio.schema._

object APIExamples extends ZIOAppDefault {
  import RouteCodec._
  import QueryCodec._

  final case class Post(id: Int, content: String)
  object Post {
    implicit val schema: Schema[Post] = DeriveSchema.gen[Post]
  }

  final case class User(id: Int, name: String)
  object User {
    implicit val schema: Schema[User] = DeriveSchema.gen[User]
  }

  // MiddlewareSpec can be added at the service level as well
  val getUsers =
    EndpointSpec.get("users" / int).out[User]

  val getUserEndpoint =
    getUsers.implement { id =>
      ZIO.succeed(User(id, "Sherlock Holmes"))
    }

  val getUserPosts =
    EndpointSpec
      .get("users" / int / "posts" / int)
      .query(queryInt("limit"))
      .out[Chunk[Post]]

  val getUserPostsEndpoint =
    getUserPosts.implement[Any, Nothing] { case (userId: Int, postId: Int, queryLimit: Int) =>
      ZIO.debug(s"API2 RESULT parsed: users/$userId/posts/$postId?limit=$queryLimit") *>
        ZIO.succeed(Chunk(Post(postId, "My first Post!")))
    }

  val middlewareSpec =
    MiddlewareSpec.auth

  // just like api.handle
  val middleware =
    middlewareSpec.implementIncoming(_ => ZIO.unit)

  val serviceSpec =
    (getUsers.toServiceSpec ++ getUserPosts.toServiceSpec).middleware(middlewareSpec)

  val app = serviceSpec.toHttpApp(getUserEndpoint ++ getUserPostsEndpoint, middleware)

  val request = Request.get(url = URL.fromString("/users/1").toOption.get)
  println(s"Looking up $request")

  val run = Server.serve(app).provide(Server.default)

  object Client {
    def example(client: Client) = {
      val registry =
        EndpointRegistry(URL.fromString("http://localhost:8080").getOrElse(???), serviceSpec)

      val executor: EndpointExecutor[Any, Any, getUsers.type with getUserPosts.type] =
        EndpointExecutor(client, registry, ZIO.succeed(Auth.Credentials("user", "pass")))

      val x1 = getUsers(42)
      val x2 = getUserPosts(42, 200, 10)

      val result1 = executor(x1)
      val result2 = executor(x2)

      result1.zip(result2)
    }
  }
}
