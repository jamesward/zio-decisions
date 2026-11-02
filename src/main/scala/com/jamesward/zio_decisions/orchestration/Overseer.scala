package com.jamesward.zio_decisions.orchestration

import com.jamesward.zio_decisions.{DecisionModel, Decisions}
import com.jamesward.zio_decisions.Decisions.{ChoiceCriteria, Content, DynamicAnswer, QuestionId, Question}
import com.jamesward.zio_decisions.orchestration.ProcessTree.{Kind, Node, State}
import zio.*
import zio.schema.{Schema, derived}
import zio.schema.codec.json.schemaJson

/**
 * The decision model as the process tree's overseer: for each process that has been running a while with no sign of life (a tool
 * call that hasn't answered), the decision model chooses, in one request for all of them, whether to keep waiting or to stop it. It
 * sees what the call is (its tool and arguments), how long it has run, how long calls like it took before (the same
 * tool with the same arguments' start, and the same tool), what the task it's for is, and how many times it was
 * already judged worth waiting for. A stopped call is a failed step, so its loop goes on and can try otherwise.
 */
object Overseer:

  enum Verdict derives CanEqual:
    case Wait, Stop

  /** A process to judge, with what's known to judge it by. */
  final case class Stalled(
    node: Node,
    task: Option[String],
    runningSeconds: Long,
    idleSeconds: Long,
    similar: List[Long],
    sameTool: List[Long],
    waitedBefore: Int,
  )

  final case class Judgment(id: ProcessTree.Id, verdict: Verdict, stopProbability: Double)

  private final case class CallView(
    tool: String,
    kind: String,
    arguments: Option[String],
    taskItIsFor: Option[String],
    runningForSeconds: Long,
    noSignOfLifeForSeconds: Long,
    earlierRunsOfThisCallSeconds: List[Long],
    earlierCallsOfThisToolSeconds: List[Long],
    alreadyJudgedWorthWaitingFor: Int,
  ) derives Schema

  private final case class State(situation: String, processes: Int) derives Schema

  /**
   * The stalled processes of `tree` (running `after` with no sign of life), each with its history: the ended calls of
   * the same tool, and those whose arguments start the same (by their recorded detail), their durations in seconds.
   */
  def stalled(tree: ProcessTree, after: Duration, waited: Map[ProcessTree.Id, Int] = Map.empty): UIO[List[Stalled]] =
    for
      now   <- Clock.instant
      all   <- tree.snapshot
      stuck <- tree.stalled(after, Some(Kind.Call))
    yield
      val byId = all.map(n => n.id -> n).toMap
      def task(n: Node): Option[String] =
        n.parent.flatMap(byId.get) match
          case Some(p) if p.kind == Kind.Task => p.detail.orElse(Some(p.name))
          case Some(p)                        => task(p)
          case None                           => None
      val ended = all.filter(n => n.kind == Kind.Call && n.state == ProcessTree.State.Done)
      def key(n: Node) = n.detail.map(_.take(60))
      stuck.toList.map: n =>
        Stalled(
          n, task(n), n.runningMs(now) / 1000, n.idleMs(now) / 1000,
          ended.filter(e => e.name == n.name && key(e) == key(n)).map(_.runningMs(now) / 1000).toList.takeRight(10),
          ended.filter(_.name == n.name).map(_.runningMs(now) / 1000).toList.takeRight(10),
          waited.getOrElse(n.id, 0),
        )

  /** What kind of call it is, as far as its name and arguments say: a whole test suite or a build runs for minutes. */
  private def kindOf(tool: String, detail: Option[String]): String =
    val n = tool.toLowerCase
    val d = detail.getOrElse("").toLowerCase
    if List("test", "build", "compile", "check", "gradle", "sbt", "mvn").exists(w => n.contains(w) || d.contains(w)) then
      if d.contains("\"full\"") || d.contains("testfull") || d.contains("gradlew test\"") || d.contains("clean")
      then "a whole build or test suite: often several minutes"
      else "a build or test run: from seconds to minutes"
    else if List("read", "search", "find", "list", "get_", "edit", "patch", "write").exists(n.contains) then "a quick call: seconds"
    else "a call"

  /** the decision model's verdict on each of `stalled` (wait for all when there are none to judge). */
  def judge(stalled: List[Stalled]): ZIO[DecisionModel, Throwable, List[Judgment]] =
    if stalled.isEmpty then ZIO.succeed(Nil)
    else
      val criteria = ChoiceCriteria(
        "wait" -> ("keep waiting: it's within what calls like it take (a first build, a dependency download, a long test " +
          "suite), or there's no history and it isn't far past what such a command plausibly needs"),
        "stop" -> ("stop it: it's far past what calls like it took, or it's a call that should be quick (a read, a search, " +
          "an edit), or it looks like it waits for something that won't come (input, a server that never answers, a race " +
          "that never ends); its loop goes on and can try otherwise"),
      ).toOption.get
      val questions = stalled.zipWithIndex.map: (s, i) =>
        QuestionId(f"call_$i%03d") -> Question.Choice(
          Content(CallView(s.node.name, kindOf(s.node.name, s.node.detail), s.node.detail, s.task, s.runningSeconds, s.idleSeconds, s.similar, s.sameTool, s.waitedBefore)).json,
          criteria,
        )
      AgentLoop.ask(Content(State("An agent's tool calls that haven't answered for a while. For each, decide: keep waiting or stop it.",
          stalled.size)), questions).map: (answers, _, _) =>
        stalled.zipWithIndex.map: (s, i) =>
          answers.get(QuestionId(f"call_$i%03d")) match
            case Some(DynamicAnswer.Choice(a)) =>
              val stop = a.probabilities.get("stop").map(_.unwrap).getOrElse(if a.choice == "stop" then 1.0 else 0.0)
              Judgment(s.node.id, if a.choice == "stop" then Verdict.Stop else Verdict.Wait, stop)
            case _ => Judgment(s.node.id, Verdict.Wait, 0.0)
