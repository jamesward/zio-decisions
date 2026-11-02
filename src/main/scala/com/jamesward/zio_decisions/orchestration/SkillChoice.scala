package com.jamesward.zio_decisions.orchestration

import com.jamesward.zio_decisions.DecisionModel
import com.jamesward.zio_decisions.Decisions.{Content, DynamicAnswer, NoulCriteria, Question, QuestionId}
import zio.*
import zio.schema.{Schema, derived}
import zio.schema.codec.json.schemaJson

/**
 * The decision model chooses which of the session's skills to load for a request, before the work starts: one request, one yes-or-no
 * question per skill, each shown by its name and description (and whether the project's instructions name it), never
 * its text. The chosen are read into the context of every decision and writing; the others stay offered, by name and
 * description, as documents the loop can read when it needs one.
 */
object SkillChoice:

  /** A skill as the decision model is asked about it. */
  final case class Offered(name: String, description: String, named: Boolean) derives Schema

  /** At most this many skills are loaded: the likeliest. */
  val MaxLoaded = 3

  /** A skill is loaded at or above this probability. */
  val Threshold = 0.5

  private final case class Asked(question: String, skill: Offered, note: Option[String]) derives Schema

  private final case class About(request: String, skills: Int, project: Option[String]) derives Schema

  private val NamedNote = "The project's agent instructions name this skill (they say to follow it for work in this project)."

  /** The indices of `skills` to load for `prompt`, likeliest first then in their order; none asked when there are none. */
  def choose(prompt: String, skills: List[Offered], context: Option[String] = None): ZIO[DecisionModel, Throwable, List[Int]] =
    if skills.isEmpty then ZIO.succeed(Nil)
    else
      val criteria = NoulCriteria(
        "the request's work is what this skill guides (writing or reviewing the code, docs or builds it's about, using the " +
          "tool it explains): following it would change how the work is done",
        "the request needs nothing this skill says: a question it doesn't bear on, other code or tools, or no work at all " +
          "(a greeting, a status question)",
      )
      val questions = skills.zipWithIndex.map: (s, i) =>
        QuestionId(f"skill_$i%03d") -> Question.Noul(
          Content(Asked("Should this skill be loaded for the request?", s.copy(description = s.description.take(1000)),
            Option.when(s.named)(NamedNote))).json,
          criteria,
        )
      AgentLoop.ask(Content(About(prompt.take(4000), skills.size, context.map(_.take(1000)))), questions).map: (answers, _, _) =>
        val scored = skills.indices.toList.flatMap(i =>
          answers.get(QuestionId(f"skill_$i%03d")).collect { case DynamicAnswer.Noul(p) if p.unwrap >= Threshold => i -> p.unwrap })
        scored.sortBy(-_._2).take(MaxLoaded).map(_._1).sorted
