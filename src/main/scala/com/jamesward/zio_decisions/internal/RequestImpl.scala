package com.jamesward.zio_decisions.internal

import com.jamesward.zio_decisions.DecisionModel
import com.jamesward.zio_decisions.Decisions.*
import zio.*

/** Implements typed and runtime-sized requests: ask the environment's [[DecisionModel]], then check that every
  * registered question has an answer of its kind. */
private[zio_decisions] object RequestImpl:

  /** Execute the compile-time NamedTuple request. The one cast bridges the
    * runtime-verified tuple back to the exact `AnswersOf[Values]` shape. */
  def run[Names <: Tuple, Values <: Tuple](
    req: AskRequest[Names, Values],
  ): ZIO[DecisionModel, Error, Result[NamedTuple.NamedTuple[Names, AnswersOf[Values]]]] =
    decide(req.state, req.model, req.entries).flatMap: decision =>
      ZIO.fromEither(answers(req.entries, decision.answers)).map: answers =>
        Result(
          answers = Tuple.fromArray(answers.map(plain).toArray).asInstanceOf[NamedTuple.NamedTuple[Names, AnswersOf[Values]]],
          model = decision.model,
          usage = decision.usage,
        )

  /** Execute a runtime-sized question batch, each answer keyed by the caller's QuestionId. */
  def runDynamic(req: DynamicAskRequest): ZIO[DecisionModel, Error, Result[Map[QuestionId, DynamicAnswer]]] =
    decide(req.state, req.model, req.entries).flatMap: decision =>
      ZIO.fromEither(answers(req.entries, decision.answers)).map: answers =>
        Result(req.entries.map(_._1).zip(answers).toMap, decision.model, decision.usage)

  private def decide(state: Content, model: Option[ModelId], entries: List[(QuestionId, Question[?])]) =
    ZIO.serviceWithZIO[DecisionModel](_.decide(DecisionModel.Request(state, entries, model)))

  /** Every registered question's answer, in order: missing or of another kind fails. */
  private def answers(entries: List[(QuestionId, Question[?])], answers: Map[QuestionId, DynamicAnswer])
      : Either[Error, List[DynamicAnswer]] =
    entries.foldRight[Either[Error, List[DynamicAnswer]]](Right(Nil)):
      case ((qid, question), acc) =>
        for
          rest   <- acc
          answer <- answers.get(qid).toRight(Error.MissingAnswer(qid))
          _      <- Either.cond(Canonical.matches(question, answer), (),
                      Error.MalformedAnswer(qid, s"answer type doesn't match the question that was asked: $answer"))
        yield answer :: rest

  /** The typed NamedTuple path's answer: a `Noul`'s bare probability, a choice's or score's answer. */
  private def plain(answer: DynamicAnswer): Any = answer match
    case DynamicAnswer.Noul(probability) => probability
    case DynamicAnswer.Choice(value)     => value
    case DynamicAnswer.Score(value)      => value
