package com.jamesward.zio_decisions

import com.jamesward.zio_decisions.Decisions.*
import com.jamesward.zio_decisions.openai.OpenAI
import com.jamesward.zio_decisions.orchestration.AgentLoop
import zio.*
import zio.http.Client
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

/**
 * OpenAI's Decisions API, live, through `OpenAI.live` (`OPENAI_API_KEY`): the shared happy-path scenarios every provider
 * passes, each kind of question answered with its probabilities, a bad key failing as an error not worth retrying, and
 * the orchestration's agent loop driven by OpenAI instead of Jev. Paid; runs only when `OPENAI_API_KEY` is set (the
 * project's `.env` sets it for tests, through sbt-dotenv).
 */
object OpenAIIntegrationSpec extends ZIOSpecDefault:

  private def decide(state: String, questions: (String, Question[?])*) =
    DecisionModel.decide(Content(state), questions.toList.map((id, q) => QuestionId(id) -> q))

  private val provider = suite("OpenAI as a decision model")(
    test("OpenAI.live is OpenAI's default model") {
      ZIO.serviceWith[DecisionModel](m => assertTrue(m.name == s"openai:${OpenAI.DefaultModel.unwrap}"))
    },
    test("a predicate: the clear yes and the clear no in one request, each by its id, with the usage") {
      for decision <- decide("A file listing: build.sbt, src/main/scala/Main.scala, README.md",
                        "lists_scala" -> Question.Noul("The listing has a Scala source file.",
                          NoulCriteria("a file ends in .scala", "no file ends in .scala")),
                        "lists_python" -> Question.Noul("The listing has a Python source file.",
                          NoulCriteria("a file ends in .py", "no file ends in .py")))
      yield
        def p(id: String) = decision.answers.get(QuestionId(id)).collect { case DynamicAnswer.Noul(p) => p.unwrap }
        assertTrue(p("lists_scala").exists(_ > 0.8), p("lists_python").exists(_ < 0.2), decision.usage.inputTokens > 0)
    },
    test("a choice: the option that fits, with a probability for every option") {
      for
        criteria <- ZIO.fromEither(ChoiceCriteria(
                      "read" -> "read a file whose path is known", "search" -> "search for text whose file isn't known",
                      "edit" -> "change a file"))
        decision <- decide("Request: show me what's in build.sbt. Nothing has been done yet.",
                      "next" -> Question.Choice("The agent's next step.", criteria))
      yield
        val answer = decision.answers.get(QuestionId("next")).collect { case DynamicAnswer.Choice(a) => a }
        assertTrue(answer.exists(_.choice == "read"), answer.exists(_.probabilities.keySet == Set("read", "search", "edit")))
    },
    test("a score: a level over the question's ordered levels, its legend the levels' descriptions") {
      for
        criteria <- ZIO.fromEither(ScoreCriteria("nothing of what was asked is done", "part of it is done",
                      "all of it is done and checked"))
        decision <- decide("Request: fix the failing test. The agent patched the bug, ran the tests: all passed.",
                      "progress" -> Question.Score("How much of the request is done.", criteria))
      yield
        val answer = decision.answers.get(QuestionId("progress")).collect { case DynamicAnswer.Score(a) => a }
        assertTrue(answer.exists(_.score > 1.4), answer.exists(_.legend.get(2).contains("all of it is done and checked")))
    },
    test("the agent loop runs on OpenAI: it reads the file the request names, then answers") {
      val read = AgentLoop.Tool("read_file", "Reads a file whose path is known, returning its text.",
        Json.Obj("type" -> Json.Str("object"), "properties" -> Json.Obj("path" -> Json.Obj("type" -> Json.Str("string"))),
          "required" -> Json.Arr(Json.Str("path"))))
      val llm = new AgentLoop.Llm:
        def arguments(request: AgentLoop.ArgumentsRequest) = ZIO.succeed(orchestration.LlmResult(Json.Obj("path" -> Json.Str("build.sbt"))))
        def reply(request: AgentLoop.ReplyRequest) = ZIO.succeed(orchestration.LlmResult(s"done after ${request.steps.size} step(s)"))
      for
        calls  <- Ref.make(Vector.empty[String])
        tools   = new AgentLoop.Tools:
                    def call(name: String, arguments: Json.Obj): Task[Json] =
                      calls.update(_ :+ name).as(Json.Str("scalaVersion := \"3.10.0\""))
        result <- AgentLoop.run("Show me what's in build.sbt.", List(read), tools, llm)
        made   <- calls.get
      yield assertTrue(made == Vector("read_file"), result.ending == AgentLoop.Ending.Answered, result.decisions.nonEmpty)
    },
  )

  private val shared = suite("shared scenarios")(SharedSpec.happyPathScenarios.map(s => test(s.name)(s.run))*)

  def spec = suite("OpenAI Decisions API, live")(
    (provider + shared).provideSomeShared[Scope](Client.default, OpenAI.live),
    test("a bad key fails as Authentication, which isn't retried") {
      decide("anything", "q" -> Question.Noul("Anything.")).exit
        .provideSome[Client](OpenAI.layer(OpenAI.ApiKey("sk-not-a-key")))
        .map(exit => assertTrue(exit.causeOption.flatMap(_.failureOption).exists(e =>
          e.isInstanceOf[Error.Authentication] && !Error.isTransient(e))))
    }.provide(Client.default),
  ) @@ ifEnvSet("OPENAI_API_KEY") @@ withLiveClock @@ withLiveSystem @@ timeout(3.minutes) @@ sequential
