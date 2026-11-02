package com.jamesward.zio_decisions.internal

import com.jamesward.zio_decisions.DecisionModel
import com.jamesward.zio_decisions.Decisions.*
import zio.json.ast.Json

/**
 * The provider-neutral canonical JSON of a decision request and its answers: what [[com.jamesward.zio_decisions.Middleware]]
 * and exchange observers see, whichever provider answered. It is TypeSafe AI's System One shape (which TypeSafe's
 * provider sends as is):
 *
 * {{{
 * {"state": …, "model": "…", "questions": {"<id>": {"type": "noul"|"choice"|"score", "instructions": …, "criteria": …}}}
 * {"model": "…", "answers": {"<id>": {"type": "noul", "noul": 0.9} | {"type": "choice", …} | {"type": "score", …}},
 *  "usage": {"input_tokens": 1, "output_tokens": 0}}
 * }}}
 */
private[zio_decisions] object Canonical:

  /** The request: `state` as given, each question's `instructions` encoded with the `Schema` it was built with. */
  def request(state: Content, model: String, entries: List[(QuestionId, Question[?])]): Json =
    Json.Obj(
      "state"     -> state.json,
      "model"     -> Json.Str(model),
      "questions" -> Json.Obj(entries.map((qid, q) => qid.unwrap -> question(q))*),
    )

  def question(q: Question[?]): Json =
    q match
      case n: Question.Noul[s] =>
        val fields = List("type" -> Json.Str("noul"), "instructions" -> instructions(n)) ++ noulCriteria(n.criteria)
        Json.Obj(fields*)
      case c: Question.Choice[s] =>
        Json.Obj(
          "type"         -> Json.Str("choice"),
          "instructions" -> instructions(c),
          "criteria"     -> Json.Obj(c.criteria.options.toSeq.map((name, desc) => name -> contentOrNull(desc))*),
        )
      case s: Question.Score[s] =>
        Json.Obj(
          "type"         -> Json.Str("score"),
          "instructions" -> instructions(s),
          "criteria"     -> Json.Arr(s.criteria.levels.map(contentOrNull)*),
        )

  /** A question's instructions as JSON, encoded with the `Schema` captured when it was built. */
  def instructions(q: Question[?]): Json = q match
    case n: Question.Noul[s]   => Codecs.toJsonAst(n.instructions)(using n.schema)
    case c: Question.Choice[s] => Codecs.toJsonAst(c.instructions)(using c.schema)
    case s: Question.Score[s]  => Codecs.toJsonAst(s.instructions)(using s.schema)

  private def noulCriteria(criteria: NoulCriteria | Null): List[(String, Json)] =
    criteria match
      case null => Nil
      case nc: NoulCriteria =>
        List("criteria" -> Json.Obj("true" -> contentOrNull(nc.whenTrue), "false" -> contentOrNull(nc.whenFalse)))

  def contentOrNull(c: Content | Null): Json =
    c match
      case null             => Json.Null
      case content: Content => content.json

  /** The answers, in canonical form. */
  def decision(decision: DecisionModel.Decision): Json =
    Json.Obj(
      "model"   -> Json.Str(decision.model.unwrap),
      "answers" -> Json.Obj(decision.answers.toSeq.map((id, a) => id.unwrap -> answer(a))*),
      "usage"   -> Json.Obj(
        "input_tokens"  -> Json.Num(decision.usage.inputTokens),
        "output_tokens" -> Json.Num(decision.usage.outputTokens),
      ),
    )

  def answer(answer: DynamicAnswer): Json = answer match
    case DynamicAnswer.Noul(p) =>
      Json.Obj("type" -> Json.Str("noul"), "noul" -> Json.Num(p.unwrap))
    case DynamicAnswer.Choice(a) =>
      Json.Obj(
        "type"          -> Json.Str("choice"),
        "choice"        -> Json.Str(a.choice),
        "probabilities" -> Json.Obj(a.probabilities.toSeq.map((k, p) => k -> Json.Num(p.unwrap))*),
        "confidence"    -> Json.Num(a.confidence.unwrap),
      )
    case DynamicAnswer.Score(a) =>
      Json.Obj(
        "type"          -> Json.Str("score"),
        "score"         -> Json.Num(a.score),
        "legend"        -> Json.Obj(a.legend.toSeq.sortBy(_._1).map((k, v) => k.toString -> Json.Str(v))*),
        "probabilities" -> Json.Obj(a.probabilities.toSeq.sortBy(_._1).map((k, p) => k.toString -> Json.Num(p.unwrap))*),
        "confidence"    -> Json.Num(a.confidence.unwrap),
      )

  /** Whether `answer` is of `question`'s kind. */
  def matches(question: Question[?], answer: DynamicAnswer): Boolean = (question, answer) match
    case (_: Question.Noul[?], _: DynamicAnswer.Noul)     => true
    case (_: Question.Choice[?], _: DynamicAnswer.Choice) => true
    case (_: Question.Score[?], _: DynamicAnswer.Score)   => true
    case _                                                => false
