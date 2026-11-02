package com.jamesward.zio_decisions

import com.jamesward.zio_decisions.DecisionModel
import com.jamesward.zio_decisions.Decisions.*
import com.jamesward.zio_decisions.typesafe.TypeSafe
import zio.*
import zio.http.{Client as HttpClient, Header, Method, Request, Response, Routes, Server, URL, handler}
import zio.json.*
import zio.json.ast.Json
import zio.schema.{Schema, derived}
import zio.test.*

/** `TypeSafe.layer` with an endpoint: requests go to that origin, with the bearer token, on the standard path. */
object EndpointSpec extends ZIOSpecDefault:

  case class State(prompt: String) derives Schema

  def spec = suite("TypeSafe.layer with an endpoint")(
    test("sends the request to the given origin") {
      for
        seen   <- Promise.make[Nothing, (String, Option[String], Json.Obj)]
        routes  = Routes(
                    Method.POST / "v1" / "systemone" -> handler { (request: Request) =>
                      request.body.asString.orDie.flatMap: body =>
                        val json = body.fromJson[Json.Obj].toOption.get
                        val questions = json.get("questions").flatMap(_.asObject).toList.flatMap(_.fields.map(_._1))
                        val answers = Json.Obj(questions.map(q => q -> Json.Obj("type" -> Json.Str("noul"), "noul" -> Json.Num(0.9)))*)
                        seen.succeed((request.url.path.encode, request.header(Header.Authorization).map(_.renderedValue), json)) *>
                          ZIO.succeed(Response.json(Json.Obj(
                            "model" -> Json.Str("jev-test"),
                            "answers" -> answers,
                            "usage" -> Json.Obj("input_tokens" -> Json.Num(3), "output_tokens" -> Json.Num(1)),
                          ).toJson))
                    },
                  )
        port   <- Server.install(routes)
        url     = URL.decode(s"http://127.0.0.1:$port").toOption.get
        answer <- ask(State("is it sunny?"), (sunny = Question.Noul("Is it sunny?"))).answers
                    .provideSome[HttpClient](TypeSafe.layer(TypeSafe.ApiKey("test-key"), ModelId("jev-test"), url))
        got    <- seen.await
      yield assertTrue(
        answer.sunny.unwrap > 0.5,
        got._1 == "/v1/systemone",
        got._2.contains("Bearer test-key"),
        got._3.get("state").exists(_.toJson.contains("is it sunny?")),
      )
    },
  ).provide(Server.defaultWithPort(0), HttpClient.default) @@ TestAspect.withLiveClock
