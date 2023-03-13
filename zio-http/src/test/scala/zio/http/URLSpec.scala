/*
 * Copyright 2021 - 2023 Sporta Technologies PVT LTD & the ZIO HTTP contributors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.http

import zio.Chunk
import zio.test.Assertion._
import zio.test._

import zio.http.internal.HttpGen
import zio.http.model.Scheme

object URLSpec extends ZIOSpecDefault {
  def spec =
    suite("URL")(
      suite("encode-decode symmetry")(
        test("auto-gen") {
          check(HttpGen.url) { url =>
            val expected        = url.normalize
            val expectedEncoded = expected.encode
            val actual          = URL.fromString(url.encode).map(_.normalize)
            val actualEncoded   = actual.map(_.encode)

            assertTrue(actualEncoded == Right(expectedEncoded)) &&
            assertTrue(actual == Right(expected))
          }
        },
        test("manual") {
          val urls = Gen.fromIterable(
            Seq(
              "",
              "/",
              "/users?ord=ASC&txt=scala%20is%20awesome%21&u=1&u=2",
              "/users",
              "/users#the%20hash",
              "http://abc.com",
              "http://abc.com/",
              "http://abc.com/list",
              "http://abc.com/users?ord=ASC&txt=scala%20is%20awesome%21&u=1&u=2",
              "http://abc.com/users?u=1&u=2&ord=ASC&txt=scala%20is%20awesome%21",
              "http://abc.com/users?u=1#the%20hash",
              "http://abc.com/users",
              "http://abc.com/users/?u=1&u=2&ord=ASC&txt=scala%20is%20awesome%21",
              "http://abc.com/users#the%20hash",
              "ws://abc.com/subscriptions",
              "wss://abc.com/subscriptions",
            ),
          )

          checkAll(urls) { url =>
            val decoded = URL.fromString(url)
            val encoded = decoded.map(_.encode)
            assertTrue(encoded == Right(url))
          }
        },
      ),
      suite("fromString")(
        test("should Handle invalid url String with restricted chars") {
          val actual = URL.fromString("http://mw1.google.com/$[level]/r$[y]_c$[x].jpg")
          assert(actual)(isLeft)
        },
      ),
      suite("relative")(
        test("converts an url to a relative url") {
          val actual   = URL.fromString("http://abc.com/users?a=1&b=2").map(_.relative.normalize.encode)
          val expected = Right("/users?a=1&b=2")
          assertTrue(actual == expected)
        },
      ),
      suite("setPath")(
        test("updates the path without needed to know the host") {
          val host     = "http://abc.com"
          val channels = "/channels"
          val users    = "/users"
          val actual   = URL.fromString(host + users).map(_.setPath(channels).normalize.encode)
          val expected = Right(host + channels)
          assertTrue(actual == expected)
        },
      ),
      suite("builder")(
        test("creates a URL with all attributes set") {
          val builderUrl = URL.empty
            .setHost("www.abc.com")
            .setPath("/list")
            .setPort(8080)
            .setScheme(Scheme.HTTPS)
            .setQueryParams("?type=builder&query=provided")

          assertTrue(builderUrl.normalize.encode == "https://www.abc.com:8080/list?query=provided&type=builder")
        },
        test("returns relative URL if port, host, and scheme are not set") {
          val actual = URL.empty
            .setPath(Path.decode("/list"))
            .setQueryParams(QueryParams(Map("type" -> Chunk("builder"), "query" -> Chunk("provided"))))
            .normalize
            .encode

          assertTrue(actual == "/list?query=provided&type=builder")
        },
      ),
      suite("java interop")(
        test("can not create a java.net.URL from a relative URL") {
          check(HttpGen.genRelativeURL) { url =>
            assert(url.toJavaURL)(isNone)
          }
        },
        test("converts a zio.http.URL to java.net.URI") {
          check(HttpGen.genAbsoluteURL) { url =>
            val httpURLString = url.encode
            val javaURLString = url.toJavaURI.toString
            assertTrue(httpURLString == javaURLString)
          }
        },
      ),
      suite("hostWithOptionalPort")(
        test("does not add the port 80 for http") {
          assertTrue(
            URL.fromString("http://localhost:80").toOption.flatMap(_.hostWithOptionalPort) == Some("localhost"),
          )
        },
        test("adds the port 8080 for http") {
          assertTrue(
            URL.fromString("http://localhost:8080").toOption.flatMap(_.hostWithOptionalPort) == Some("localhost:8080"),
          )
        },
        test("does not add the port 443 for https") {
          assertTrue(
            URL.fromString("https://localhost:443").toOption.flatMap(_.hostWithOptionalPort) == Some("localhost"),
          )
        },
        test("adds the port 80 for https") {
          assertTrue(
            URL.fromString("https://localhost:80").toOption.flatMap(_.hostWithOptionalPort) == Some("localhost:80"),
          )
        },
      ),
    )
}
