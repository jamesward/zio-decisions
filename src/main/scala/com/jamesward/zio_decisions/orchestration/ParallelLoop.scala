package com.jamesward.zio_decisions.orchestration

import com.jamesward.zio_decisions.{DecisionModel, Decisions}
import com.jamesward.zio_decisions.Decisions.{Content, DynamicAnswer, LoopTurn, NoulCriteria, Question, QuestionId}
import com.jamesward.zio_decisions.orchestration.AgentLoop.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.{Schema, derived}
import zio.schema.codec.json.schemaJson

/**
 * Work on a request in parallel, when it's made of independent parts, coordinated through one append-only log of what
 * every loop does, rather than separate copies of the work to reconcile afterwards:
 *
 *   1. gather: one loop reads what the request needs known before it can be planned (the spec, the docs and files it
 *      names), changing nothing; its calls are the log's first events, and every later loop starts from them.
 *   2. plan: the generative model, given the request and what was gathered, proposes the request's parts (each a task
 *      one agent can do and check on its own) and any setup they all need; the decision model decides, with one yes-or-no question,
 *      whether they're independent enough to do at the same time. When not (or there's one part), one [[AgentLoop]]
 *      does the request, from what was gathered.
 *   3. setup (when the plan has any): one loop makes the changes every part needs first (a shared helper, a
 *      dependency).
 *   4. fork: one [[AgentLoop]] per part, all at once, each told its part, the others', and to change only what its
 *      part needs. Every loop's context is a projection of the log ([[view]]), read again at each decision: the
 *      gathered and setup work, and what the other parts have done so far, fitted to the request's budget by their
 *      priorities ([[Sections]]). A part whose loop fails is that part's outcome; the others go on. A part that
 *      doesn't finish (unable, out of steps, failed) gets another round at once, from its first attempt's calls
 *      ([[Config.retries]]).
 *   5. join: one loop checks the parts together (the whole build and test suite, fixing what doesn't fit) and writes
 *      the reply.
 *
 * The log ([[Event]]s, in [[Result.events]]) is the record: the result's steps are its events in order. Coordinating
 * the tools themselves (one build at a time, merged test runs) is the host's, in the [[AgentLoop.Tools]] it gives;
 * [[currentLoop]] tells it which loop a call is for. Interrupting the run interrupts every loop.
 */
object ParallelLoop:

  /**
   * The loop a fiber works for ("gather", "setup", a part's name, "integration", "main"; `None` outside a [[ParallelLoop]]), for the
   * host's tools to coordinate by: whose edit, whose test run.
   */
  val currentLoop: FiberRef[Option[String]] = Unsafe.unsafely(FiberRef.unsafe.make(Option.empty[String]))

  /**
   * Runs `zio` for loop `name`, within the loop the fiber already works for (`task-2/scenario 1`), its log lines
   * annotated `loop=<that>`.
   */
  private def within[R, A](name: String)(zio: ZIO[R, Throwable, A]): ZIO[R, Throwable, A] = Work.loop(name)(zio)

  /** A plan entry's state, for [[Config.progress]]. */
  enum Progress derives CanEqual:
    case Pending, InProgress, Completed

  /** One part of a request: what to call it, and the task its loop is given. */
  final case class Part(name: String, task: String) derives Schema, JsonCodec

  /** The parts the generative model proposed, and the shared work they all need first. */
  final case class Plan(parts: List[Part], shared: String) derives Schema, JsonCodec

  object Plan:
    /** The plan's JSON Schema, for structured output. */
    val schema: Json.Obj =
      val string = Json.Obj("type" -> Json.Str("string"))
      Json.Obj(
        "type" -> Json.Str("object"),
        "properties" -> Json.Obj(
          "shared" -> Json.Obj("type" -> Json.Str("string"), "description" -> Json.Str(
            "Changes every part needs before it starts, made once (a shared helper, a dependency), or empty: the reading is done")),
          "parts" -> Json.Obj(
            "type" -> Json.Str("array"),
            "description" -> Json.Str("The request's independent parts, each a task one agent can do on its own; one part when it can't be split"),
            "items" -> Json.Obj(
              "type" -> Json.Str("object"),
              "properties" -> Json.Obj(
                "name" -> Json.Obj("type" -> Json.Str("string"), "description" -> Json.Str(
                  "A short name that says which part it is: scenario 1, roman, the shipping change")),
                "task" -> Json.Obj("type" -> Json.Str("string"), "description" -> Json.Str(
                  "The part as a task for its agent: what to do, where, and how to check it (the one test to run)")),
              ),
              "required" -> Json.Arr(Json.Str("name"), Json.Str("task")),
            ),
          ),
        ),
        "required" -> Json.Arr(Json.Str("parts"), Json.Str("shared")),
      )

  /** What the generative model needs to propose a plan: the request, the project, the tools, and what was gathered. */
  final case class PlanRequest(prompt: String, context: Option[String], tools: Seq[Tool], gathered: Option[String] = None)

  /** The generative model's planning, besides what [[AgentLoop.Llm]] does. */
  trait Planner:
    def plan(request: PlanRequest): Task[LlmResult[Plan]]

  /** One event of the log: a loop ("gather", "setup", a part's name, "integration", "main") made a call, and how it ended. */
  final case class Event(sequence: Int, loop: String, tool: String, arguments: Json.Obj, outcome: Outcome)

  /**
   * @param maxParts    plans with more parts are cut to this many
   * @param partSteps   each part's loop's tool-call budget
   * @param sharedSteps the gathering's budget, and the setup's
   * @param joinSteps   the integration loop's budget
   * @param draftFirst  one loop first writes every part in one go and runs the tests once (most of a request's parts
   *                    are often small enough to get right in one pass, and one pass costs one test run); only the parts
   *                    it didn't get right are worked on at the same time
   * @param draftSteps  the draft's budget
   * @param progress    told the run's plan as it goes (every entry, in order, with its state, each time one changes):
   *                    what an ACP agent reports as its plan
   * @param judgeRetry  a part that didn't finish gets another round only when the decision model, from the round's calls, judges one
   *                    likely to get there (a fresh start from what it learned), not when it went round in circles
   * @param partTimeout how long one round of a part may take: a part still at work then ends (as unable: it's
   *                    stopped, its calls so far in the log) so a part that can't get there doesn't hold up the turn
   * @param workTools   which tools the part and integration loops are offered (not, say, a tool that shows a file to
   *                    the user, which does no work for an agent working alone)
   * @param readOnly    which tools only read (the gathering is offered only those, so it can't start the work; other
   *                    loops' reads rank below their edits in a loop's view); by default, those annotated `readOnlyHint`
   * @param retries     how many more rounds a part that doesn't finish gets, each as soon as the one before ends
   * @param maxParallel how many parts' loops work at once (the rest wait their turn: [[Work]])
   * @param filterTools each gathering, part and integration loop first asks the decision model which tools its task could need
   *                    ([[AgentLoop.Strategy.relevanceFilter]], one request each), so its decisions offer those (and an
   *                    option for the rest) rather than every tool: a part's task is narrow, and a large toolset's
   *                    options are most of every request
   */
  final case class Config(
    loop: AgentLoop.Config = AgentLoop.Config(),
    maxParts: Int = 12,
    partSteps: Int = 20,
    sharedSteps: Int = 5,
    joinSteps: Int = 12,
    filterTools: Boolean = true,
    retries: Int = 1,
    readOnly: Tool => Boolean = Config.annotatedReadOnly,
    workTools: Tool => Boolean = _ => true,
    draftFirst: Boolean = true,
    draftSteps: Int = 8,
    partTimeout: Option[Duration] = Some(5.minutes),
    judgeRetry: Boolean = true,
    progress: List[(String, Progress)] => UIO[Unit] = _ => ZIO.unit,
    maxParallel: Int = Work.DefaultParallelism,
  ):
    /** The loop config of a gathering, part or integration loop of at most `steps` calls. */
    def phase(steps: Int): AgentLoop.Config =
      loop.copy(maxSteps = steps, strategy = loop.strategy.copy(relevanceFilter = loop.strategy.relevanceFilter || filterTools))

    /** A part's loop config: its budget, and when to end. */
    def part: AgentLoop.Config =
      val c = phase(partSteps)
      c.copy(instructions = c.instructions + "\n\n" + PartEnding)

  /** What a part's loop is told about ending, besides the loop's own instructions. */
  val PartEnding: String =
    "This loop does one part of a larger request, alongside other agents. End with an answer as soon as the part's own " +
      "test has passed after its last edit (a shared run's result says which of your tests passed): don't read or check " +
      "more. Don't wait for, check or fix the other parts. End as unable only when the part can't be done."

  object Config:
    val annotatedReadOnly: Tool => Boolean = _.annotations.exists(_.get("readOnlyHint").contains(Json.Bool(true)))

  /** The result: the reply and how it ended, the plan, the log, and what every model used, all loops together. */
  final case class Result(
    reply: String,
    ending: Ending,
    plan: Option[Plan],
    events: Vector[Event],
    decisions: List[LoopTurn],
    decisionUsage: Decisions.Usage,
    llmCalls: Int,
    llmInputTokens: Long,
    llmOutputTokens: Long,
    llmLatencyMs: Long,
    toolLatencyMs: Long,
    parts: List[(String, Ending, Long)] = Nil,
  ):
    def steps: Vector[Step] = events.zipWithIndex.map((e, i) => Step(i + 1, e.tool, e.arguments, e.outcome))
    def decisionLatencyMs: Long = decisions.map(_.latencyMs).sum

  private final case class SplitAbout(question: String, request: String, parts: List[Part], shared: String) derives Schema

  /** Runs `prompt` as [[ParallelLoop]] describes; one loop when the plan has one part or the decision model won't split it. */
  def run(
    prompt: String,
    tools: Seq[Tool],
    call: Tools,
    llm: Llm & Planner,
    config: Config = Config(),
    documents: Seq[Document] = Nil,
    context: Option[String] = None,
    live: UIO[Sections] = ZIO.succeed(Sections.empty),
  ): ZIO[DecisionModel, Throwable, Result] =
    val readOnly = tools.filter(config.readOnly).map(_.name).toSet
    // the gathering reads only: offered the read-only tools (all of them when none is known to be)
    val reads = if readOnly.isEmpty then tools else tools.filter(t => readOnly(t.name))
    for
      log      <- Ref.make(Vector.empty[Event])
      gathered <- within("gather")(AgentLoop.run(
                    s"$prompt\n\nBefore this request's work is planned: read what it needs known to see what its parts are and " +
                      "how to do them (the spec or documentation it points to, the files it names, the code to change). Read " +
                      "and look only; change nothing; keep to what the request says not to read. Read each thing once (a long " +
                      "page in its parts; a project file with the file tools, not the web). End with an answer as soon as you know " +
                      "(the work comes after, by other agents; you can't change anything here).",
                    reads, logging(log, "gather", call), silent(llm), config.phase(config.sharedSteps), documents, context, live,
                  )).either
      _        <- gathered.fold(e => ZIO.logWarning(s"the gathering failed: ${e.getMessage}"), _ => ZIO.unit)
      seen     <- log.get
      known     = view(seen, "plan", readOnly).fit(PlanBudget).renderOption
      planned  <- llm.plan(PlanRequest(prompt, context, tools, known)).either
      _        <- planned.fold(e => ZIO.logWarning(s"no plan: ${e.getMessage}"), _ => ZIO.unit)
      plan      = planned.toOption.map(_.value).map(normalize(_, config.maxParts))
      before    = gathered.toOption.fold(Totals())(r => Totals() + r.copy(llmCalls = r.llmCalls - 1)) ++
                    planned.toOption.map(Totals(_)).toList
      split    <- plan.filter(_.parts.size > 1).fold(ZIO.succeed((false, Option.empty[LoopTurn])))(p => splits(prompt, p))
      result   <- (plan, split) match
                    case (Some(p), (true, turn)) =>
                      parallel(prompt, p, tools, call, llm, config, documents, context, live, log, before ++ turn)
                    case (_, (_, turn)) =>
                      within("main")(AgentLoop.run(prompt, tools, logging(log, "main", call), llm, config.loop, documents, context,
                        live.zipWith(log.get)((outside, events) => outside ++ view(events, "main", readOnly)))).flatMap: r =>
                        log.get.map(events => (before ++ turn + r).result(r.reply, r.ending, plan, events, Nil))
    yield result

  /**
   * The request worked on as it needs, with no mode to choose: the decision model first answers, in one yes-or-no question (one cheap
   * request), whether the request is made of several independent parts of real work. When not (a question, a bug, one
   * change), it's one [[AgentLoop]] from the start, with no gathering or planning. When so, it's [[run]]: gathered,
   * planned, drafted in one pass, and only the parts the draft didn't get right worked on at the same time.
   */
  def auto(
    prompt: String,
    tools: Seq[Tool],
    call: Tools,
    llm: Llm & Planner,
    config: Config = Config(),
    documents: Seq[Document] = Nil,
    context: Option[String] = None,
    live: UIO[Sections] = ZIO.succeed(Sections.empty),
  ): ZIO[DecisionModel, Throwable, Result] =
    for
      // without tools there's no work to share out: no question
      decided <- if tools.isEmpty then ZIO.fail(IllegalStateException("no tools")).either else severalParts(prompt, context).either
      several  = decided.toOption.exists(_._1)
      _       <- ZIO.logInfo(s"auto: ${if several then "several parts: parallel" else "one loop"}" +
                   decided.fold(e => s" (The decision model couldn't decide: ${e.getMessage})", d => f" (p ${d._3}%.2f)"))
      turn     = decided.toOption.map(_._2)
      result  <-
        if several then run(prompt, tools, call, llm, config, documents, context, live).map(r => r.copy(decisions = turn.toList ++ r.decisions,
          decisionUsage = turn.fold(r.decisionUsage)(t => Decisions.Usage(r.decisionUsage.inputTokens + t.usage.inputTokens, r.decisionUsage.outputTokens + t.usage.outputTokens))))
        else
          for
            log <- Ref.make(Vector.empty[Event])
            r   <- within("main")(AgentLoop.run(prompt, tools, logging(log, "main", call), llm, config.loop, documents, context, live))
            evs <- log.get
          yield (Totals() ++ turn + r).result(r.reply, r.ending, None, evs, Nil)
    yield result

  private final case class SeveralAbout(request: String, project: Option[String]) derives Schema

  /** the decision model's answer: is the request several independent parts of real work? With its turn, and its probability. */
  def severalParts(prompt: String, context: Option[String]): ZIO[DecisionModel, Throwable, (Boolean, LoopTurn, Double)] =
    val id = QuestionId("several_parts")
    val question = Question.Noul(
      Content("Is this request made of several independent parts of real work, each needing its own code and its own test, " +
        "that agents could do at the same time?").json,
      NoulCriteria(
        "it asks for several separate things to be built or changed (scenarios, endpoints, functions, katas, unrelated " +
          "changes), each with its own code and check",
        "it's one thing: a question, a bug to find and fix, one feature or change, an explanation; or its steps depend " +
          "on each other",
      ),
    )
    AgentLoop.ask(Content(SeveralAbout(prompt, context.map(_.take(4000)))), List(id -> question)).map: (answers, usage, latency) =>
      val p = answers.get(id).collect { case DynamicAnswer.Noul(p) => p.unwrap }.getOrElse(0.0)
      val answer = Decisions.ChoiceAnswer(if p >= 0.5 then "several" else "one", Map.empty, Decisions.Probability(p).toOption.get)
      (p >= 0.5, LoopTurn(0, answer.choice, answer, usage, latency), p)

  /** How many characters of the gathered work the planner gets. */
  val PlanBudget = 40000

  extension (t: Totals)
    private def ++(more: List[Totals]): Totals = more.foldLeft(t)((a, b) => a.combine(b))

  /** A plan's parts with a task, uniquely named (a repeated name numbered), at most `max`. */
  def normalize(plan: Plan, max: Int): Plan =
    val parts = plan.parts.filter(p => p.task.trim.nonEmpty).map(p => p.copy(name = p.name.trim.filter(c => !c.isControl)))
      .map(p => if p.name.isEmpty then p.copy(name = "part") else p)
    val named = parts.foldLeft(Vector.empty[Part]): (acc, p) =>
      val taken = acc.map(_.name).toSet
      val name = if !taken(p.name) then p.name else LazyList.from(2).map(n => s"${p.name} ($n)").find(n => !taken(n)).get
      acc :+ p.copy(name = name)
    plan.copy(parts = named.take(max).toList)

  /** the decision model's answer: are these parts independent enough to work on at the same time? */
  private def splits(prompt: String, plan: Plan): ZIO[DecisionModel, Throwable, (Boolean, Option[LoopTurn])] =
    val id = QuestionId("split")
    val question = Question.Noul(
      Content(SplitAbout("Can these parts be worked on at the same time, each by its own agent, without waiting for each other?",
        prompt, plan.parts, plan.shared)).json,
      NoulCriteria(
        "each part can be done and checked on its own, given the shared work: it changes its own code (its own method or " +
          "class, even when several parts' methods are in one file) and has its own test",
        "a part needs another part's result before it can start, or two parts must change the same lines, or the request " +
          "is really one task",
      ),
    )
    AgentLoop.ask(Content("Plan the request's work"), List(id -> question)).map: (answers, usage, latency) =>
      val p = answers.get(id).collect { case DynamicAnswer.Noul(p) => p.unwrap }.getOrElse(0.0)
      val answer = Decisions.ChoiceAnswer(if p >= 0.5 then "split" else "one", Map.empty, Decisions.Probability(p).toOption.get)
      (p >= 0.5, Some(LoopTurn(0, answer.choice, answer, usage, latency)))

  private def parallel(
    prompt: String,
    plan: Plan,
    tools: Seq[Tool],
    call: Tools,
    llm: Llm,
    config: Config,
    documents: Seq[Document],
    context: Option[String],
    live: UIO[Sections],
    log: Ref[Vector[Event]],
    before: Totals,
  ): ZIO[DecisionModel, Throwable, Result] =
    val readOnly = tools.filter(config.readOnly).map(_.name).toSet
    val work = tools.filter(config.workTools)
    // what's changing around the whole run (the user's steering), with each loop's projection of the log
    def and(self: String, log: Ref[Vector[Event]]): UIO[Sections] =
      live.zipWith(log.get)((outside, events) => outside ++ view(events, self, readOnly))
    val entries =
      List("Read what the request needs") ++ Option.when(plan.shared.trim.nonEmpty)("Set up what every part needs").toList ++
        Option.when(config.draftFirst)("Draft every part, and run the tests once").toList ++
        plan.parts.map(p => s"${p.name}: ${p.task.take(120)}") ++ List("Check the parts together, and reply")
    for
      states  <- Ref.make(entries.map(_ -> Progress.Pending).updated(0, entries.head -> Progress.Completed))
      mark     = (i: Int, st: Progress) => states.updateAndGet(es => if i >= 0 && i < es.size then es.updated(i, es(i)._1 -> st) else es).flatMap(config.progress)
      index    = (prefix: String) => entries.indexWhere(_.startsWith(prefix))
      _       <- states.get.flatMap(config.progress)
      _       <- mark(index("Set up"), Progress.InProgress).when(plan.shared.trim.nonEmpty)
      shared  <- ZIO.when(plan.shared.trim.nonEmpty):
                   within("setup")(AgentLoop.run(
                     s"$prompt\n\nThe request is split into parts (${plan.parts.map(_.name).mkString(", ")}) that agents will do at " +
                       s"the same time. First, only the setup every part needs, done once for all of them: ${plan.shared}\n" +
                       "Do just that (not any part's own work), check it compiles, and end with an answer.",
                     tools, logging(log, "setup", call), silent(llm), config.phase(config.sharedSteps), documents, context,
                     and("setup", log),
                   )).either
      names    = plan.parts.map(_.name)
      // draft first: one loop writes every part in one go and runs the tests once; the parts it didn't get right (The decision model
      // judges each, from what that run showed) are the ones worked on at the same time
      once    <- testsOnce(call, readOnly)
      _       <- mark(index("Set up"), Progress.Completed) *> mark(index("Draft"), Progress.InProgress)
      drafted <- ZIO.when(config.draftFirst):
                   within("draft")(AgentLoop.run(draftTask(prompt, plan), work, logging(log, "draft", once), silent(llm),
                     config.phase(config.draftSteps), documents, context, and("draft", log))).either
      judged  <- drafted.flatMap(_.toOption).fold(ZIO.succeed(Set.empty[String])): _ =>
                   within("draft")(log.get.flatMap(events => finished(prompt, plan, events, logging(log, "draft", call), readOnly, "draft")))
      // most parts failing for one shared cause (the same error in code they share): one loop fixes that first, rather
      // than every part's loop at once
      repair  <- ZIO.when(config.draftFirst && drafted.exists(_.isRight) && judged.size * 2 < plan.parts.size)(log.get.flatMap(events =>
                   sharedCause(prompt, plan, events, readOnly)))
      repaired <- ZIO.foreach(repair.flatten): cause =>
                   ZIO.logInfo(s"parallel: one shared cause first: ${cause.take(200)}") *>
                     within("repair")(AgentLoop.run(repairTask(prompt, plan, cause), work, logging(log, "repair", testsOnce0(call, readOnly)),
                       silent(llm), config.phase(config.draftSteps), documents, context, and("repair", log))).either
      done    <- repaired.flatMap(_.toOption).fold(ZIO.succeed(judged)): _ =>
                   within("repair")(log.get.flatMap(events => finished(prompt, plan, events, logging(log, "repair", call), readOnly, "repair")))
                     .map(_ ++ judged)
      _       <- ZIO.logInfo(s"parallel: the draft did ${done.toList.sorted.mkString(", ")}").when(config.draftFirst)
      open     = plan.parts.filterNot(p => done(p.name))
      // each open part's specification, the gathered paragraphs the decision model judges it needs, verbatim: the planner's task is a
      // summary, and a summary can be wrong
      specs   <- log.get.flatMap(events => ZIO.foreachPar(open)(p => specFor(p, paragraphs(events)).map(p.name -> _))).map(_.toMap)
      // a part that fails (its loop, not its tools: those failures are steps) is that part's outcome, not the run's
      _       <- ZIO.logInfo(s"parallel: ${open.size} parts (${open.map(_.name).mkString(", ")})")
      // each part's loop, and when it doesn't finish (unable, out of steps, its loop failed) another round at once, from
      // what its first attempt did: a part's retry doesn't wait for the others, and the integration doesn't fix it
      _       <- mark(index("Draft"), Progress.Completed)
      _       <- ZIO.foreachDiscard(plan.parts.filter(p => done(p.name)))(p => mark(index(s"${p.name}:"), Progress.Completed))
      // every open part at the same time ([[Work]]): each its own loop and time limit, and, when it doesn't finish
      // (unable, out of steps, its loop failed), another round at once from what it did, when the decision model judges one likely to
      // get there; a part someone stopped isn't tried again
      runs    <- Work.all(open.map(part => partItem(prompt, part, names, 0, None, work, call, llm, config, documents, context, live, log, specs, readOnly)),
                   config.maxParallel, (name, st) => mark(index(s"$name:"), st).unit)
      parts    = plan.parts.flatMap: part =>
                   runs.find(_.name == part.name).map(d => PartRun(part.name, d.last.result, d.ms))
                     .orElse(drafted.flatMap(_.toOption).map(d => PartRun(part.name, Right(d), 0)))
                     .toList
      _       <- mark(entries.size - 1, Progress.InProgress)
      join    <- within("integration")(AgentLoop.run(
                   s"$prompt\n\nThe request's parts (${names.mkString(", ")}) were each done by an agent at the same time:\n" +
                     parts.map(_.summary).mkString("\n") + "\n" +
                     "Check them together: build the project and run its whole test suite; fix what fails (a part not done, parts " +
                     "that don't fit together), and run it again. Then answer the request: what was done, part by part, and how " +
                     "it was checked.",
                   work, logging(log, "integration", call), llm, config.phase(config.joinSteps), documents, context,
                   and("integration", log),
                 ))
      _       <- mark(entries.size - 1, Progress.Completed)
      events  <- log.get
    yield
      // the gathering and part loops' replies are the silent model's, not calls
      val quiet = (shared.flatMap(_.toOption).toList ++ drafted.flatMap(_.toOption).toList ++ runs.flatMap(_.rounds).flatMap(_.result.toOption))
        .map(r => r.copy(llmCalls = r.llmCalls - 1))
      val totals = before ++ quiet + join
      totals.result(join.reply, join.ending, Some(plan), events, parts.map(p => (p.name, p.ending, p.ms)))

  /** A part's loop: how it ended (a failed loop is [[Ending.Unable]]), and how long it took. */
  private final case class PartRun(name: String, result: Either[Throwable, AgentLoop.Result], ms: Long):
    def ending: Ending = result.fold(_ => Ending.Unable, _.ending)
    def summary: String = result match
      case Right(r) => s"- \"$name\": ${r.ending}, ${r.steps.size} tool calls"
      case Left(s: ProcessTree.Stopped) => s"- \"$name\": stopped (${s.reason}); leave it unless the request needs it"
      case Left(e)  => s"- \"$name\": its agent failed (${Option(e.getMessage).getOrElse(e.toString).take(200)}); do it yourself"

  /** A part's task: the request, its part, the others, and how to work beside them. */
  def partTask(prompt: String, part: Part, names: List[String]): String =
    val others = names.filterNot(_ == part.name)
    s"$prompt\n\nYou are one of ${names.size} agents working on this request at the same time. Yours is " +
      s"\"${part.name}\": ${part.task}\nThe others work on ${others.map(n => s"\"$n\"").mkString(", ")} at the " +
      "same time: change only what your part needs. Other parts edit the same files at the same time, so put your " +
      "part's code where no other part works: a new file of its own (a class named for your part) when the language " +
      "allows, and in a shared file only the smallest change that hooks it in (one line calling it), with a patch whose " +
      "context is your part's own lines. Read a shared file once, around your part's lines, not whole. Don't change " +
      "the setup's or another part's code. Check only your part (run just its test; a run that " +
      "hangs holds up the others' runs, so give it a short timeout where the tool takes one), and end with an answer " +
      "when it's done and checked. The shared work's results are in the context; don't redo it. When the build fails " +
      "in code another part is writing (not yours), don't edit that code: its agent fixes it; fix only errors in your own " +
      "code, and run your test again."

  /** Whether a call is a test or command run (not a read, an edit, a build). */
  private def isRun(tool: String, readOnly: Set[String]): Boolean =
    !readOnly(tool) && List("command", "test", "terminal", "run_", "gradle", "sbt").exists(tool.contains)

  /**
   * `call` with one test run: a second fails, saying the draft's test run is done (the draft's work is a first pass;
   * the parts it didn't get right are the parts' loops' to fix).
   */
  private def testsOnce(call: Tools, readOnly: Set[String]): UIO[Tools] =
    Ref.make(false).map: ran =>
      (name, arguments) =>
        if !isRun(name, readOnly) then call.call(name, arguments)
        else ran.getAndSet(true).flatMap: before =>
          if before then ZIO.fail(RuntimeException("not run: the draft runs the tests once, and that run is done; end with an answer " +
            "saying which parts passed (the others are worked on next, each by its own agent)"))
          else call.call(name, arguments)

  /** [[testsOnce]], made at once (for a loop made inside a `for`). */
  private def testsOnce0(call: Tools, readOnly: Set[String]): Tools =
    val ran = Unsafe.unsafely(Ref.unsafe.make(false))
    (name, arguments) =>
      if !isRun(name, readOnly) then call.call(name, arguments)
      else ran.getAndSet(true).flatMap: before =>
        if before then ZIO.fail(RuntimeException("not run: this loop runs the tests once, and that run is done; end with an answer"))
        else call.call(name, arguments)

  private final case class CauseAbout(question: String, parts: List[String], failures: List[String], latestTestRun: String) derives Schema

  /**
   * A test run's failures grouped by their cause: each failed test's first cause line (its exception and message),
   * numbers and test names masked, with how many tests failed that way, most common first.
   */
  def causes(output: String): List[(String, Int)] =
    val text = output.replace("\\n", "\n")
    val lines = text.linesIterator.toVector
    val failed = lines.indices.filter(i => """\(\)\s+FAILED""".r.findFirstIn(lines(i)).isDefined)
    val firsts = failed.map: i =>
      lines.drop(i + 1).map(_.trim).find(l => l.nonEmpty && !l.startsWith("at ")).getOrElse("(no cause shown)")
    firsts.map(l => l.replaceAll("""scenario\d+|test\w*\d+""", "<test>").replaceAll("""\d+""", "N").take(160))
      .groupBy(identity).toList.map((c, xs) => c -> xs.size).sortBy(-_._2)

  /** The question whether a run's failures share one cause, with the failures grouped by cause for the decision model to compare. */
  def sharedCauseQuestion(parts: List[String], output: String): (QuestionId, Question[?]) =
    val all = causes(output)
    val total = all.map(_._2).sum
    val grouped = all.map((c, n) => s"$n of $total failed with: $c")
    QuestionId("shared_cause") -> Question.Noul(
      Content(CauseAbout("Do most of these parts fail for one shared cause, the same failure in code they share (a helper, " +
        "a setup, the build), rather than each for its own reasons? The failures are grouped by their cause: one group " +
        "holding most of them (well over half) is one shared cause; several groups of a similar size are not.", parts, grouped,
        digest(output, 6000))).json,
      NoulCriteria("most failures are the same (one large group: the same error, the same timeout, the same stack in shared code)",
        "they differ: many small groups, each part failing its own way, or the output doesn't show a common cause"),
    )
  private final case class Cause(cause: String) derives Schema

  /**
   * Whether the draft's parts failed for one shared cause, by the decision model's yes-or-no from the draft's last test run, and
   * that run's digest as the cause to fix; none when they failed each its own way, or there was no run.
   */
  private def sharedCause(prompt: String, plan: Plan, events: Vector[Event], readOnly: Set[String]): ZIO[DecisionModel, Nothing, Option[String]] =
    events.filter(_.loop == "draft").findLast(e => isRun(e.tool, readOnly) && e.outcome.isInstanceOf[Outcome.Succeeded]) match
      case None => ZIO.none
      case Some(run) =>
        val output = run.outcome match
          case Outcome.Succeeded(r) => r.asString.getOrElse(r.toJson)
          case Outcome.Failed(m)    => m
        val d = digest(output)
        val (id, question) = sharedCauseQuestion(plan.parts.map(_.name), output)
        AgentLoop.ask(Content(prompt), List(id -> question)).map: (answers, _, _) =>
          Option.when(answers.get(id).exists { case DynamicAnswer.Noul(p) => p.unwrap >= 0.5; case _ => false })(d)
        .catchAll(e => ZIO.logWarning(s"couldn't judge the draft's failures: ${e.getMessage}").as(None))

  /** The repair's task: the one cause most parts fail for, fixed in the shared code, checked with one test run. */
  def repairTask(prompt: String, plan: Plan, cause: String): String =
    s"$prompt\n\nA first draft of every part (${plan.parts.map(_.name).mkString(", ")}) is in the files, and most of its tests " +
      "fail the same way, for one shared cause. Find that cause in the code the parts share (a helper, the setup) and fix " +
      "it there, once; don't rewrite the parts. Then run the tests once and end with an answer saying which parts pass. " +
      s"The draft's test run, in short:\n${cause.take(6000)}"

  /** The draft's task: every part, in one go, checked with one test run. */
  def draftTask(prompt: String, plan: Plan): String =
    s"$prompt\n\nThe request's parts: ${plan.parts.map(p => s"\"${p.name}\"").mkString(", ")}. Write a first version of " +
      "every part now, in as few edits as you can (one patch for several parts where they're in one file), then run the " +
      "tests once (all of the parts' tests in one run) and end with an answer saying which parts passed. Don't fix the " +
      "ones that failed, and don't run the tests again: other agents take the failed parts, each on its own, at the " +
      "same time. Fix only what keeps the code from compiling."

  private final case class FinishedAbout(question: String, part: String, task: String, latestTestRun: String) derives Schema

  /**
   * The parts the draft got right: the decision model's yes-or-no per part, in one request, from the draft's last test run (the last
   * of its build, test or command calls); none when there was no run.
   */
  private def finished(prompt: String, plan: Plan, events: Vector[Event], call: Tools, readOnly: Set[String], loop: String)
      : ZIO[DecisionModel, Nothing, Set[String]] =
    val draft = events.filter(_.loop == loop)
    def isRun(e: Event) = ParallelLoop.isRun(e.tool, readOnly) && e.outcome.isInstanceOf[Outcome.Succeeded]
    def isEdit(e: Event) = !readOnly(e.tool) && !isRun(e) && !e.tool.contains("build") && !e.tool.contains("problems")
    draft.lastIndexWhere(isRun) match
      case -1 => ZIO.succeed(Set.empty)
      case i =>
        val run = draft(i)
        // the draft edited after its last run: the run is made again, so the judgment is of the code as it is
        val latest =
          if draft.drop(i + 1).exists(isEdit) then
            call.call(run.tool, run.arguments).fold(e => Option(e.getMessage).getOrElse(""), r => r.asString.getOrElse(r.toJson))
          else ZIO.succeed(run.outcome match
            case Outcome.Succeeded(r) => r.asString.getOrElse(r.toJson)
            case Outcome.Failed(m)    => m)
        latest.flatMap: output =>
          val ids = plan.parts.zipWithIndex.map((p, i) => QuestionId(f"part_$i%03d") -> p)
          val questions = ids.map: (id, p) =>
            id -> Question.Noul(
              Content(FinishedAbout("Did this part's own test pass in the latest test run?", p.name, p.task, digest(output))).json,
              NoulCriteria("the run shows this part's test passing", "it failed, didn't run, or the run doesn't show it"),
            )
          AgentLoop.ask(Content(prompt), questions).map: (answers, _, _) =>
            ids.collect { case (id, p) if answers.get(id).exists { case DynamicAnswer.Noul(q) => q.unwrap >= 0.7; case _ => false } => p.name }.toSet
          .catchAll(e => ZIO.logWarning(s"couldn't judge the draft: ${e.getMessage}").as(Set.empty))

  /**
   * A test run's output in short: its per-test result lines (any line saying a test passed or failed: Gradle's
   * `Class > test() PASSED`, munit's `+ name`, pytest's `PASSED`) and its summary, without the stack traces between
   * them, which are most of a failing run's output.
   */
  def digest(output: String, max: Int = 12000): String =
    val text = output.replace("\\n", "\n")
    val lines = text.linesIterator.toVector
    val result = """(?i)\b(passed|failed|skipped|pass|fail|ok|error)\b|^\s*[+x✓✗×]\s|tests? (completed|run)|BUILD (SUCCESSFUL|FAILED)""".r
    // each result line, and after a failure its cause: the next lines that aren't stack frames, up to two
    val cause = """(?i)exception|error|timed out|expected|but was|assert""".r
    val kept = lines.indices.flatMap: i =>
      val l = lines(i)
      if result.findFirstIn(l).isEmpty || l.trim.startsWith("at ") || l.length >= 300 then Vector.empty
      else
        val after = if "(?i)fail".r.findFirstIn(l).isEmpty then Vector.empty
          else lines.drop(i + 1).takeWhile(n => !n.trim.startsWith("at ") && result.findFirstIn(n).forall(_ => cause.findFirstIn(n).isDefined))
            .filter(n => cause.findFirstIn(n).isDefined).take(2).map(n => "    " + n.trim.take(240))
        l +: after
    val head = kept.mkString("\n")
    val tail = text.takeRight(1500)
    (if head.length + tail.length <= max then s"$head\n…\n$tail" else head.take(max - tail.length) + s"\n…\n$tail")

  /**
   * A part's round as an item of [[Work]]: its loop, under its time limit, seeing its specification and the log; when it
   * doesn't end answered and rounds are left, another round, from what this one did, when the decision model judges one likely to help.
   */
  private def partItem(prompt: String, part: Part, names: List[String], n: Int, earlier: Option[PartRun], work: Seq[Tool], call: Tools,
      llm: Llm, config: Config, documents: Seq[Document], context: Option[String], live: UIO[Sections], log: Ref[Vector[Event]],
      specs: Map[String, String], readOnly: Set[String]): Work.Item[DecisionModel, AgentLoop.Result] =
    val name = if n == 0 then part.name else s"${part.name} (again)"
    val task = earlier.fold(partTask(prompt, part, names))(e => retryTask(prompt, part, names, e.summary))
    val previous = Option.when(n > 0)(part.name)
    Work.Item(
      name,
      Work.Kind.Loop,
      AgentLoop.run(task, work, logging(log, name, call), silent(llm), config.part, documents, context,
        live.zipWith(log.get)((outside, events) =>
          Sections.of("The specification of your part, verbatim (where your task's wording differs, this wins)",
            specs.getOrElse(part.name, ""), SpecPriority, 4000) ++ outside ++ view(events, name, readOnly, previous))),
      timeout = config.partTimeout,
      succeeded = Some(_.ending == Ending.Answered),
      again = Some: round =>
        if n >= config.retries then ZIO.none
        else
          val run = PartRun(part.name, round.result, round.ms)
          (if config.judgeRetry then log.get.flatMap(events => worthAnother(part, events.filter(_.loop == name))) else ZIO.succeed(true))
            .flatMap: again =>
              if again then ZIO.logInfo(s"parallel: another round for ${part.name}")
                .as(Some(partItem(prompt, part, names, n + 1, Some(run), work, call, llm, config, documents, context, live, log, specs, readOnly)))
              else ZIO.logInfo(s"parallel: no other round for ${part.name}: the decision model judged it unlikely to help").as(None),
      // a part's sub-agent works toward its part's task
      goal = Some(s"${part.name}: ${part.task}"),
    )

  private final case class RetryAbout(question: String, part: String, task: String, calls: List[String]) derives Schema

  /** the decision model's answer: would another round of `part`, starting from what this one did, likely finish it? */
  private def worthAnother(part: Part, round: Vector[Event]): ZIO[DecisionModel, Nothing, Boolean] =
    val id = QuestionId("another_round")
    val calls = round.toList.takeRight(24).map: e =>
      s"${e.tool} ${e.arguments.toJson.take(200)} → " + (e.outcome match
        case Outcome.Succeeded(r) => digest(r.asString.getOrElse(r.toJson), 600)
        case Outcome.Failed(m)    => s"failed: ${m.take(300)}")
    AgentLoop.ask(Content("Decide whether to give a part of the work another round"), List(id -> Question.Noul(
      Content(RetryAbout("This part's agent ended without finishing it. Would another agent, starting fresh from what this one " +
        "did and learned, likely finish it?", part.name, part.task, calls)).json,
      NoulCriteria("yes: it got close, or it failed for a reason a fresh look would fix (a wrong reading, a fixable error)",
        "no: it went round in circles, the same failure every time, with no new idea of why"),
    ))).map: (answers, _, _) =>
      answers.get(id).forall { case DynamicAnswer.Noul(p) => p.unwrap >= 0.4; case _ => true }
    .catchAll(_ => ZIO.succeed(true))

  /** A part's round that took longer than its limit: it's tried again (unlike a part someone stopped). */
  type TimedOut = Work.TimedOut
  val TimedOut = Work.TimedOut

  /** A part's specification's priority among the context's sections: above the shared work it's taken from. */
  val SpecPriority = 70

  /**
   * The gathered documents' text, in paragraphs: each gathered result's long strings, cut at blank lines and before
   * each numbered item, short ones joined to the next, at most [[MaxParagraphs]].
   */
  def paragraphs(events: Vector[Event]): Vector[String] =
    def strings(j: Json): List[String] = j match
      case Json.Str(t)      => List(t)
      case Json.Obj(fields) => fields.toList.flatMap(f => strings(f._2))
      case Json.Arr(items)  => items.toList.flatMap(strings)
      case _                => Nil
    val texts = events.filter(_.loop == "gather").flatMap:
      case Event(_, _, _, _, Outcome.Succeeded(r)) => strings(r).filter(_.length >= 400)
      case _                                       => Nil
    texts.flatMap: t =>
      val pieces = t.split("""\n\s*\n|\n(?=\d{1,3}\. )""").map(_.trim).filter(_.nonEmpty).toVector
      pieces.foldLeft(Vector.empty[String]): (acc, p) =>
        acc.lastOption match
          case Some(last) if last.length < 120 => acc.dropRight(1) :+ s"$last\n\n$p"
          case _                               => acc :+ p
      .map(p => if p.length <= 2500 then p else p.take(2500) + " …")
    .distinct.take(MaxParagraphs)

  val MaxParagraphs = 120

  private final case class SpecAbout(part: String, task: String) derives Schema
  private final case class ParagraphAbout(question: String, paragraph: String) derives Schema

  /**
   * The paragraphs the decision model judges `part` needs (what it implements, its interface, its test, conventions it must follow):
   * one yes-or-no question per paragraph, in one request; nothing when there are none or the decision model can't be asked.
   */
  def specFor(part: Part, paragraphs: Vector[String]): ZIO[DecisionModel, Nothing, String] =
    if paragraphs.isEmpty then ZIO.succeed("")
    else
      val ids = paragraphs.zipWithIndex.map((p, i) => QuestionId(f"para_$i%03d") -> p)
      val questions = ids.toList.map: (id, p) =>
        id -> Question.Noul(
          Content(ParagraphAbout("Does the agent doing this part need this paragraph: what the part must do, its " +
            "interface or format, its test, or a convention it must follow? (Not paragraphs about the other parts.)", p)).json,
          NoulCriteria("it states something this part must do or follow", "it's about other parts, or general background"),
        )
      AgentLoop.ask(Content(SpecAbout(part.name, part.task)), questions).map: (answers, _, _) =>
        ids.collect { case (id, p) if answers.get(id).exists { case DynamicAnswer.Noul(q) => q.unwrap >= 0.5; case _ => false } => p }
          .mkString("\n\n")
      .catchAll(e => ZIO.logWarning(s"couldn't pick ${part.name}'s specification: ${e.getMessage}").as(""))

  /** A part's second round: the part, what its first attempt came to, and that its calls are in the context. */
  def retryTask(prompt: String, part: Part, names: List[String], earlier: String): String =
    partTask(prompt, part, names) + "\n\nThis part was attempted once already and didn't finish (" + earlier.stripPrefix("- ") +
      "). Its calls are in the context as your earlier attempt: start from where it got to (what it changed is in the " +
      "files), find what was wrong (read the failing test's output, the code as it is now), and finish the part."

  /** The loop's calls, each an event of the log as it ends. */
  private def logging(log: Ref[Vector[Event]], loop: String, call: Tools): Tools =
    (name, arguments) =>
      call.call(name, arguments).exit.flatMap: exit =>
        val outcome = exit.foldExit(c => Outcome.Failed(Option(c.squash.getMessage).getOrElse(c.squash.toString)), Outcome.Succeeded(_))
        log.update(l => l :+ Event(l.size + 1, loop, name, arguments, outcome)) *> exit

  /** The loops whose work is every loop's: the gathering and the setup. */
  val SharedLoops: Set[String] = Set("gather", "setup", "draft", "repair")

  /** The priorities of a loop's view of the log, among the context's sections (the project context's is 40). */
  val SharedPriority = 60
  val EarlierPriority = 50
  val OthersEditPriority = 30
  val OthersPriority = 20

  /**
   * A loop's projection of the log (pure): each shared-work call as a section of its own (high priority, kept to
   * 3000 characters before it's dropped, since the spec it read is what every part needs), and each other loop's call
   * in short, edits above reads (an edit changes the files this loop works in), newer above older. The loop's own calls
   * aren't in it: they're its steps. Fitted to a budget, older reads go first, then older edits, then the project
   * context, then shared work.
   */
  def view(events: Vector[Event], self: String, readOnly: Set[String], earlier: Option[String] = None): Sections =
    def cut(s: String, n: Int) = if s.length <= n then s else s.take(n) + s"… (${s.length - n} more)"
    def result(o: Outcome, n: Int) = o match
      case Outcome.Succeeded(r) => cut(r.asString.getOrElse(r.toJson), n)
      case Outcome.Failed(m)    => s"failed: ${cut(m, n)}"
    val shared = events.filter(e => SharedLoops(e.loop) && e.loop != self).map: e =>
      val what = e.loop match
        case "setup" => "Setup for every part (done once; don't redo it)"
        case "draft" => "The first draft of every part, written before the parts were split (build on it; its code is in the files)"
        case "repair" => "A fix of a cause the draft's parts shared, made before the parts were split (its code is in the files)"
        case _       => "Gathered for the whole request (don't read it again)"
      Sections.Section(s"$what: ${e.tool} ${cut(e.arguments.toJson, 300)}", result(e.outcome, 20000), SharedPriority, 3000)
    val own = events.filter(e => earlier.contains(e.loop)).map: e =>
      Sections.Section(s"Your earlier attempt at this part: ${e.tool} ${cut(e.arguments.toJson, 600)}", result(e.outcome, 3000),
        EarlierPriority, 400)
    val others = events.filter(e => !SharedLoops(e.loop) && e.loop != self && !earlier.contains(e.loop)).map: e =>
      val priority = if readOnly(e.tool) then OthersPriority else OthersEditPriority
      Sections.Section(s"Another agent [${e.loop}], working at the same time: ${e.tool} ${cut(e.arguments.toJson, 300)}",
        result(e.outcome, 300), priority)
    Sections(shared ++ own ++ others)

  /** The generative model without the reply: a gathering or part loop's reply isn't shown to anyone. */
  private def silent(llm: Llm): Llm =
    new Llm:
      def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]] = llm.arguments(request)
      def reply(request: ReplyRequest): Task[LlmResult[String]] = ZIO.succeed(LlmResult(s"${request.ending}"))

  /** Usage summed across loops. */
  private[orchestration] final case class Totals(
    decisions: List[LoopTurn] = Nil,
    decisionIn: Int = 0,
    decisionOut: Int = 0,
    llmCalls: Int = 0,
    llmIn: Long = 0,
    llmOut: Long = 0,
    llmMs: Long = 0,
    toolMs: Long = 0,
  ):
    def +(r: AgentLoop.Result): Totals =
      Totals(decisions ++ r.decisions, decisionIn + r.decisionUsage.inputTokens, decisionOut + r.decisionUsage.outputTokens, llmCalls + r.llmCalls,
        llmIn + r.llmInputTokens, llmOut + r.llmOutputTokens, llmMs + r.llmLatencyMs, toolMs + r.toolLatencyMs)
    def ++(rs: Seq[AgentLoop.Result]): Totals = rs.foldLeft(this)(_ + _)
    def combine(o: Totals): Totals =
      Totals(decisions ++ o.decisions, decisionIn + o.decisionIn, decisionOut + o.decisionOut, llmCalls + o.llmCalls, llmIn + o.llmIn, llmOut + o.llmOut,
        llmMs + o.llmMs, toolMs + o.toolMs)
    def ++(turn: Option[LoopTurn]): Totals =
      turn.fold(this)(t => copy(decisions = decisions :+ t, decisionIn = decisionIn + t.usage.inputTokens, decisionOut = decisionOut + t.usage.outputTokens))
    def result(reply: String, ending: Ending, plan: Option[Plan], events: Vector[Event], parts: List[(String, Ending, Long)]): Result =
      Result(reply, ending, plan, events, decisions, Decisions.Usage(decisionIn, decisionOut), llmCalls, llmIn, llmOut, llmMs, toolMs, parts)

  private object Totals:
    def apply(planned: LlmResult[Plan]): Totals =
      Totals(llmCalls = 1, llmIn = planned.inputTokens, llmOut = planned.outputTokens, llmMs = planned.latencyMs)
