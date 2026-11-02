package com.jamesward.zio_decisions.orchestration

import com.jamesward.zio_decisions.{DecisionModel, Decisions}
import com.jamesward.zio_decisions.Decisions.{Content, DynamicAnswer, NoulCriteria, Question, QuestionId}
import zio.*
import zio.schema.{Schema, derived}
import zio.schema.codec.json.schemaJson

/**
 * The decision model chooses, from a log of earlier work (tasks in this session and others in the project), what matters to a new
 * request: one request, one yes-or-no question per earlier task. There's no clearing of context: everything stays in
 * the log, and each request recalls only what bears on it (a follow-up recalls the task it follows; an unrelated
 * request, nothing).
 */
object Recall:

  /** An earlier task, as the log keeps it: its request, how it ended, its reply and the files it touched. */
  final case class Earlier(
    request: String,
    reply: Option[String],
    ending: String,
    minutesAgo: Long,
    sameSession: Boolean,
    files: List[String] = Nil,
  ) derives Schema

  /** At most this many of the newest earlier tasks are asked about. */
  val MaxCandidates = 20

  /** At most this many earlier tasks are recalled: the likeliest. */
  val MaxRecalled = 5

  /** A task is recalled at or above this probability. */
  val Threshold = 0.5

  private final case class Asked(question: String, earlierTask: Earlier) derives Schema

  private final case class About(newRequest: String, earlierTasks: Int, project: Option[String]) derives Schema

  /** The indices of `earlier` that matter to `prompt`, newest of the candidates kept; none asked when there are none. */
  def recall(prompt: String, earlier: List[Earlier], context: Option[String] = None): ZIO[DecisionModel, Throwable, List[Int]] =
    val candidates = earlier.zipWithIndex.takeRight(MaxCandidates)
    if candidates.isEmpty then ZIO.succeed(Nil)
    else
      val criteria = NoulCriteria(
        "the new request continues, refers to, corrects or depends on this task (\"now add tests for it\", \"that didn't " +
          "work\", \"commit it\", the same code or feature), or its reply holds facts the new request needs",
        "the new request is about something else, stands on its own, or this task's work doesn't bear on it",
      )
      val questions = candidates.map: (e, i) =>
        QuestionId(f"task_$i%03d") -> Question.Noul(
          Content(Asked("Does this earlier task matter to the new request?", e.copy(request = e.request.take(1500),
            reply = e.reply.map(_.take(1500))))).json,
          criteria,
        )
      AgentLoop.ask(Content(About(prompt.take(4000), candidates.size, context.map(_.take(1000)))), questions).map: (answers, _, _) =>
        // the likeliest few, in their order: a related topic recalls a dozen tasks, and each is up to 3000 characters in
        // every decision's and writing's context
        val scored = candidates.flatMap((_, i) =>
          answers.get(QuestionId(f"task_$i%03d")).collect { case DynamicAnswer.Noul(p) if p.unwrap >= Threshold => i -> p.unwrap })
        scored.sortBy(-_._2).take(MaxRecalled).map(_._1).sorted
