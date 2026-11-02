package com.jamesward.zio_decisions.orchestration

import com.jamesward.zio_decisions.{DecisionModel, Decisions}
import com.jamesward.zio_decisions.Decisions.{Content, DynamicAnswer, NoulCriteria, Question, QuestionId}
import zio.*
import zio.schema.{Schema, derived}
import zio.schema.codec.json.schemaJson

/** One cheap yes-or-no judgment by the decision model, for a host's own decisions (is this worth acting on?). */
object Judge:

  private final case class About(situation: String) derives Schema

  /** the decision model's probability that the answer to `question` about `situation` is yes, by the `yes` and `no` criteria. */
  def yesNo(situation: String, question: String, yes: String, no: String): ZIO[DecisionModel, Throwable, Double] =
    val id = QuestionId("judgment")
    AgentLoop.ask(Content(About(situation)), List(id -> Question.Noul(Content(question).json, NoulCriteria(yes, no))))
      .map((answers, _, _) => answers.get(id).collect { case DynamicAnswer.Noul(p) => p.unwrap }.getOrElse(0.0))
