package com.jamesward.zio_decisions

import com.jamesward.zio_decisions.Decisions.*
import com.jamesward.zio_decisions.internal.Canonical
import zio.*
import zio.json.ast.Json

/**
 * A decision model (a "System 1" model): given a state, it answers a set of typed questions with probabilities, quickly
 * and cheaply. The questions are the provider-neutral decision language ([[Decisions.Question]]):
 *
 *   - yes or no (`Noul`): the probability that the answer is yes;
 *   - a choice among described options (`Choice`): a probability for each;
 *   - a score over described levels (`Score`).
 *
 * Everything in this library ([[Decisions.ask]], [[Decisions.loop]], the orchestration) asks through this trait, never
 * through a particular provider's client. Providers implement it: `typesafe.TypeSafe` (The decision model) and `openai.OpenAI` (the
 * Decisions API); a model of your own (or a test double) is another implementation.
 */
trait DecisionModel:
  /** The provider's id, for logs and metrics: `typesafe`, `openai`. */
  def provider: String

  /** The model a request uses unless it names one ([[DecisionModel.Request.model]]). */
  def defaultModel: ModelId

  /** Which model this is, for logs and metrics: `typesafe:jev-latest`, `openai:gpt-6-luna`. */
  def name: String = s"$provider:${defaultModel.unwrap}"

  /** One physical exchange: the answers to every one of `request`'s questions, or why there are none. */
  def decide(request: DecisionModel.Request): IO[Error, DecisionModel.Decision]

object DecisionModel:

  /** The questions to answer about `state`, by id, in order; `model` overrides the model's [[DecisionModel.defaultModel]]. */
  final case class Request(state: Content, questions: List[(QuestionId, Question[?])], model: Option[ModelId] = None):
    /** The request as provider-neutral canonical JSON (`{state, model, questions}`), for observation and logging. */
    def json(defaultModel: ModelId): Json = Canonical.request(state, model.getOrElse(defaultModel).unwrap, questions)

  /** The answers, by question, with the model that answered and the tokens the request used. */
  final case class Decision(answers: Map[QuestionId, DynamicAnswer], model: ModelId, usage: Usage):
    /** The decision as provider-neutral canonical JSON (`{model, answers, usage}`), for observation and logging. */
    def json: Json = Canonical.decision(this)

  /** One decision, from the environment's model. */
  def decide(state: Content, questions: List[(QuestionId, Question[?])]): ZIO[DecisionModel, Error, Decision] =
    ZIO.serviceWithZIO[DecisionModel](_.decide(Request(state, questions)))

  /** Decorates any decision model with exactly-once exchange observation. The underlying success value or full failure
    * cause is re-emitted unchanged after the best-effort callback. */
  def observed(observer: ExchangeObserver): URLayer[DecisionModel, DecisionModel] =
    Middleware.fromObserver(observer).layer

  /** `layer @@ middleware`: the layer's model, decorated (found through `DecisionModel`'s implicit scope). */
  extension [R, E](layer: ZLayer[R, E, DecisionModel])
    def @@(middleware: Middleware): ZLayer[R, E, DecisionModel] =
      layer >>> middleware.layer
