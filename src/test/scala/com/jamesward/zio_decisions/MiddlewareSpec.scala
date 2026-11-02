package com.jamesward.zio_decisions

import com.jamesward.zio_decisions.DecisionModel
import com.jamesward.zio_decisions.Decisions.*
import com.jamesward.zio_decisions.typesafe.Wire
import zio.*
import zio.test.*

object MiddlewareSpec extends ZIOSpecDefault:

  private val success = TypeSafeMock.MockBehavior.Respond(
    Map("answer" -> Wire.Answer.Noul(0.75)),
    Wire.Usage(7, 3),
  )

  private def program = Decisions.ask("state", (answer = Question.Noul("Valid?"))).run

  def spec = suite("decision middleware")(
    test("composes left around right like ZIO HTTP middleware") {
      for
        tracked <- TypeSafeMock.tracked(success)
        events <- Ref.make(Vector.empty[String])
        outer = Middleware.make: (_, next) =>
          events.update(_ :+ "outer-before") *>
            next.tap(_ => events.update(_ :+ "outer-after"))
        inner = Middleware.make: (_, next) =>
          events.update(_ :+ "inner-before") *>
            next.tap(_ => events.update(_ :+ "inner-after"))
        result <- program.provideLayer(tracked.layer @@ (outer ++ inner))
        seen <- events.get
      yield assertTrue(
        result.answers.answer.unwrap == 0.75,
        seen == Vector("outer-before", "inner-before", "inner-after", "outer-after"),
      )
    },
    test("can wrap and retry an exchange effect") {
      val failure = Error.RateLimit("slow down")
      for
        tracked <- TypeSafeMock.tracked(TypeSafeMock.MockBehavior.Fail(failure), success)
        middleware = Middleware.make: (_, next) =>
          next.catchSome:
            case _: Error.RateLimit => next
        result <- program.provideLayer(tracked.layer @@ middleware)
        count <- tracked.requestCount
      yield assertTrue(result.answers.answer.unwrap == 0.75, count == 2)
    },
    test("exposes canonical request and response bodies") {
      for
        tracked <- TypeSafeMock.tracked(success)
        bodies <- Ref.make(Option.empty[(zio.json.ast.Json, zio.json.ast.Json)])
        middleware = Middleware.make: (request, next) =>
          next.tap(response => bodies.set(Some(request.body -> response.body)))
        _ <- program.provideLayer(tracked.layer @@ middleware)
        seen <- bodies.get
      yield assertTrue(
        seen.flatMap(_._1.asObject).exists(_.get("questions").nonEmpty),
        seen.flatMap(_._2.asObject).exists(_.get("answers").nonEmpty),
      )
    },
  )
