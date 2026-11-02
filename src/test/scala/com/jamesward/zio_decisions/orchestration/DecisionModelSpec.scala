package com.jamesward.zio_decisions.orchestration

import com.jamesward.zio_decisions.{DecisionModel, Decisions, TypeSafeMock}
import com.jamesward.zio_decisions.DecisionModel
import com.jamesward.zio_decisions.Decisions.*
import com.jamesward.zio_decisions.typesafe.TypeSafe
import com.jamesward.zio_decisions.orchestration.AgentLoop.*
import zio.*
import zio.http.Status
import zio.json.ast.Json
import zio.test.*

/** The orchestration's decision model is any [[DecisionModel]]: Jev is one; a model of its own drives the loop as well. */
object DecisionModelSpec extends ZIOSpecDefault:

  private def p(d: Double) = Probability(d).toOption.get

  /**
   * A decision model that isn't Jev: it calls the first tool once, then ends; every yes-or-no is no. It records each
   * request's questions.
   */
  private final class Scripted(asked: Ref[Vector[List[String]]]) extends DecisionModel:
    val provider = "scripted"
    val defaultModel = ModelId("scripted")
    def decide(request: DecisionModel.Request): IO[Error, DecisionModel.Decision] =
      val questions = request.questions
      asked.updateAndGet(_ :+ questions.map(_._1.unwrap)).map: all =>
        val first = all.count(_.contains("next_action")) == 1
        val answers = questions.map:
          case (id, q: Question.Choice[?]) =>
            val keys = q.criteria.options.keys.toList.sorted
            val pick = if id.unwrap == "next_action" then (if first then keys.find(_.startsWith("tool_")) else keys.find(_ == "end_answer"))
                       else None
            val chosen = pick.getOrElse(keys.head)
            id -> (DynamicAnswer.Choice(ChoiceAnswer(chosen, keys.map(k => k -> p(if k == chosen then 1.0 else 0.0)).toMap, p(1.0))): DynamicAnswer)
          case (id, _: Question.Noul[?]) => id -> DynamicAnswer.Noul(p(0.0))
          case (id, _)                   => id -> DynamicAnswer.Noul(p(0.0))
        DecisionModel.Decision(answers.toMap, defaultModel, Decisions.Usage(7, 1))

  private val writer = new Llm:
    def arguments(request: ArgumentsRequest) = ZIO.succeed(LlmResult(Json.Obj("path" -> Json.Str("A"))))
    def reply(request: ReplyRequest) = ZIO.succeed(LlmResult(s"done after ${request.steps.size} step(s)"))

  def spec = suite("DecisionModel")(
    test("a decision model of its own drives the agent loop: its choices are the steps, its usage the decisions'") {
      val read = Tool("read_file", "Reads.", Json.Obj("type" -> Json.Str("object")))
      for
        asked  <- Ref.make(Vector.empty[List[String]])
        calls  <- Ref.make(Vector.empty[String])
        tools   = new Tools:
                    def call(name: String, arguments: Json.Obj): Task[Json] = calls.update(_ :+ name).as(Json.Str("text"))
        result <- AgentLoop.run("read A", List(read), tools, writer).provideLayer(ZLayer.succeed[DecisionModel](Scripted(asked)))
        made   <- calls.get
        all    <- asked.get
      yield assertTrue(made == Vector("read_file"), result.ending == Ending.Answered, result.reply == "done after 1 step(s)",
        all.count(_.contains("next_action")) == 2, result.decisionUsage.inputTokens == 7 * all.size)
    },
    test("TypeSafe's HTTP errors become the library's: too long, transient, failed") {
      assertTrue(
        TypeSafe.failure(Status.BadRequest, "max_tokens_exceeded").isInstanceOf[Error.TooLong],
        Error.isTransient(TypeSafe.failure(Status.TooManyRequests, "slow down")),
        Error.isTransient(TypeSafe.failure(Status.BadGateway, "x")),
        !Error.isTransient(TypeSafe.failure(Status.Unauthorized, "bad key")),
        TypeSafe.failure(Status.BadRequest, "other").isInstanceOf[Error.BadRequest],
      )
    },
    test("Jev as a decision model: named for its model, its answers passed through") {
      for
        mock   <- TypeSafeMock.tracked(TypeSafeMock.MockBehavior.Always(_ =>
                    TypeSafeMock.MockBehavior.Respond(Map("q" -> com.jamesward.zio_decisions.typesafe.Wire.Answer.Noul(0.8)))))
        named  <- ZIO.service[DecisionModel].map(_.name).provideLayer(mock.decisions)
        asked  <- AgentLoop.ask(Content("s"), List(QuestionId("q") -> Question.Noul("yes?"))).provideLayer(mock.decisions)
      yield assertTrue(named == "typesafe:mock", asked._1.get(QuestionId("q")).exists {
        case DynamicAnswer.Noul(v) => v.unwrap == 0.8
        case _                     => false
      })
    },
  )
