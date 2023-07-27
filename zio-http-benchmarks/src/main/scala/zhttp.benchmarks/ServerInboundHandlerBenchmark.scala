package zhttp.benchmarks

import zio.http.Client
import org.openjdk.jmh.annotations._
import zio._
import zio.http._

import java.util.concurrent.TimeUnit

@State(org.openjdk.jmh.annotations.Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class ServerInboundHandlerBenchmark {

  private val MAX  = 10000
  private val PAR  = 10
  private val res  = ZIO.succeed(Response.ok)
  private val http = Routes(Route.route(Method.GET / "text")(handler(res))).toHttpApp


  def benchmarkZio() =
    (for {
      _ <- zio.Console.printLine("Benchmarking start")
      _ <- Server.serve(http).fork
      _ <- zio.Console.printLine("Server started on port 8080")
      client <- ZIO.service[Client]
      _ <- client.get("/text").repeatN(MAX)
    } yield ()).provide(Server.default, ZClient.default, zio.Scope.default)

  @Benchmark
  def benchmarkApp(): Unit = {
    zio.Unsafe.unsafe(implicit u =>
      zio.Runtime.default.unsafe.run(benchmarkZio())
    )
  }

  def benchmarkZioParallel() =
    (for {
      _ <- zio.Console.printLine("Benchmarking start")
      _ <- Server.serve(http).fork
      _ <- zio.Console.printLine("Server started on port 8080")
      client <- ZIO.service[Client]
      _ <- (ZIO.foreachPar((0 until PAR).toList)(_ => client.get("/text"))).repeatN(MAX / PAR)
    } yield ()).provide(Server.default, ZClient.default, zio.Scope.default)

  @Benchmark
  def benchmarkAppParallel(): Unit = {
    zio.Unsafe.unsafe(implicit u =>
      zio.Runtime.default.unsafe.run(benchmarkZioParallel())
    )
  }
}
