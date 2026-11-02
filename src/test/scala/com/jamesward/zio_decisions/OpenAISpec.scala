package com.jamesward.zio_decisions

import com.jamesward.zio_decisions.Decisions.*
import com.jamesward.zio_decisions.openai.OpenAI
import zio.*
import zio.http.{Client as HttpClient, Header, Method, Request, Response, Routes, Server, Status, URL, handler}
import zio.json.*
import zio.json.ast.Json
import zio.test.*

/**
 * OpenAI's Decisions API provider against a fake `POST /v1/decisions`: how the library's questions go on the wire
 * (input, predicate criteria, choice options, score levels), how the answers come back, and how HTTP errors and refusals
 * become the library's errors.
 */
object OpenAISpec extends ZIOSpecDefault:

  private val department = ChoiceCriteria("billing" -> "Payments and refunds.", "other" -> null).toOption.get
  private val severity = ScoreCriteria("Cosmetic", "Blocked").toOption.get

  private val answered = Json.Obj(
    "model" -> Json.Str("gpt-6-luna"),
    "answers" -> Json.Arr(
      Json.Obj("type" -> Json.Str("predicate"), "name" -> Json.Str("urgent"), "probability" -> Json.Num(0.9)),
      Json.Obj("type" -> Json.Str("choice"), "name" -> Json.Str("department"), "choice" -> Json.Str("billing"),
        "probabilities" -> Json.Arr(
          Json.Obj("value" -> Json.Str("billing"), "probability" -> Json.Num(0.8)),
          Json.Obj("value" -> Json.Str("other"), "probability" -> Json.Num(0.2))),
        "confidence" -> Json.Num(0.7)),
      Json.Obj("type" -> Json.Str("score"), "name" -> Json.Str("severity"), "score" -> Json.Num(0.25),
        "probabilities" -> Json.Arr(
          Json.Obj("value" -> Json.Num(0), "label" -> Json.Str("Level 0"), "probability" -> Json.Num(0.75)),
          Json.Obj("value" -> Json.Num(1), "label" -> Json.Str("Level 1"), "probability" -> Json.Num(0.25))),
        "confidence" -> Json.Num(0.5)),
    ),
    "usage" -> Json.Obj("input_tokens" -> Json.Num(42)),
  )

  /** A fake Decisions API answering every request with `status` and `body`; returns its URL and the requests seen. */
  private def fake(status: Status, body: Json) =
    for
      seen  <- Ref.make(Vector.empty[(String, Option[String], Json.Obj)])
      routes = Routes(
                 Method.POST / "v1" / "decisions" -> handler { (request: Request) =>
                   request.body.asString.orDie.flatMap: text =>
                     seen.update(_ :+ (request.url.path.encode, request.header(Header.Authorization).map(_.renderedValue),
                       text.fromJson[Json.Obj].toOption.get)) *>
                       ZIO.succeed(Response.json(body.toJson).status(status))
                 },
               )
      port  <- Server.install(routes)
    yield (URL.decode(s"http://127.0.0.1:$port").toOption.get, seen)

  private def model(url: URL) = OpenAI.layer(OpenAI.ApiKey("sk-test"), OpenAI.DefaultModel, url)

  private def ask1(url: URL) =
    ask("I was charged twice and can't log in.", (
      urgent     = Question.Noul("Is it urgent?", NoulCriteria("needs action today", "can wait")),
      department = Question.Choice("Which team?", department),
      severity   = Question.Score("How severe?", severity),
    )).run.provideSome[HttpClient](model(url))

  private def failure(status: Status, body: String) =
    for
      (url, _) <- fake(status, Json.Obj("error" -> Json.Obj("message" -> Json.Str(body))))
      exit     <- ask1(url).exit
    yield exit.causeOption.flatMap(_.failureOption)

  def spec = suite("OpenAI Decisions API")(
    test("questions become OpenAI's: the input as text, predicate criteria, choice options, labeled score levels") {
      for
        (url, seen) <- fake(Status.Ok, answered)
        _           <- ask1(url)
        requests    <- seen.get
      yield
        val (path, auth, body) = requests.head
        val questions = body.get("questions").flatMap(_.asArray).get.flatMap(_.asObject)
        def named(n: String) = questions.find(_.get("name").contains(Json.Str(n))).get
        assertTrue(
          path == "/v1/decisions",
          auth.contains("Bearer sk-test"),
          body.get("model").contains(Json.Str("gpt-6-luna")),
          body.get("input").contains(Json.Str("I was charged twice and can't log in.")),
          named("urgent").get("type").contains(Json.Str("predicate")),
          named("urgent").get("instructions").contains(Json.Str("Is it urgent?\nTrue when: needs action today\nFalse when: can wait")),
          named("department").get("choices").contains(Json.Arr(
            Json.Obj("value" -> Json.Str("billing"), "description" -> Json.Str("Payments and refunds.")),
            Json.Obj("value" -> Json.Str("other"), "description" -> Json.Str("other")))),
          named("severity").get("levels").contains(Json.Arr(
            Json.Obj("label" -> Json.Str("Level 0"), "description" -> Json.Str("Cosmetic")),
            Json.Obj("label" -> Json.Str("Level 1"), "description" -> Json.Str("Blocked")))),
        )
    },
    test("answers come back typed, keyed by question, with the model and usage") {
      for
        (url, _) <- fake(Status.Ok, answered)
        result   <- ask1(url)
      yield assertTrue(
        result.answers.urgent.unwrap == 0.9,
        result.answers.department.choice == "billing",
        result.answers.department.probabilities("other").unwrap == 0.2,
        result.answers.department.confidence.unwrap == 0.7,
        result.answers.severity.score == 0.25,
        result.answers.severity.legend == Map(0 -> "Cosmetic", 1 -> "Blocked"),
        result.answers.severity.probabilities(0).unwrap == 0.75,
        result.model == OpenAI.DefaultModel,
        result.usage.inputTokens == 42, result.usage.outputTokens == 0,
      )
    },
    test("a refusal fails the decision with Error.Refused for that question") {
      val refused = Json.Obj("answers" -> Json.Arr(
        Json.Obj("type" -> Json.Str("refusal"), "name" -> Json.Str("q"))))
      for
        (url, _) <- fake(Status.Ok, refused)
        exit     <- ask("x", (q = Question.Noul("?"))).answers.provideSome[HttpClient](model(url)).exit
      yield assertTrue(exit.causeOption.flatMap(_.failureOption).contains(Error.Refused(QuestionId("q"))))
    },
    test("HTTP errors become the library's: too long, rate limited (transient), unauthorized (not)") {
      for
        tooLong <- failure(Status.BadRequest, "context_length_exceeded")
        limited <- failure(Status.TooManyRequests, "slow down")
        denied  <- failure(Status.Unauthorized, "bad key")
      yield assertTrue(
        tooLong.exists(_.isInstanceOf[Error.TooLong]),
        limited.exists(e => e.isInstanceOf[Error.RateLimit] && Error.isTransient(e)),
        denied.exists(e => e.isInstanceOf[Error.Authentication] && !Error.isTransient(e)),
      )
    },
    test("OpenAI.live without OPENAI_API_KEY fails with MissingApiKey") {
      for
        _    <- TestSystem.putEnv("OPENAI_API_KEY", "")
        exit <- ZIO.service[DecisionModel].provideSome[HttpClient](OpenAI.live).exit
      yield assertTrue(exit.causeOption.flatMap(_.failureOption).contains(Error.MissingApiKey("OPENAI_API_KEY")))
    },
  ).provide(Server.defaultWithPort(0), HttpClient.default) @@ TestAspect.withLiveClock
