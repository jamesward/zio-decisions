package com.jamesward.zio_decisions.orchestration

import com.jamesward.zio_decisions.TypeSafeMock
import com.jamesward.zio_decisions.TypeSafeMock.MockBehavior.{Always, AlwaysOr, Respond}
import com.jamesward.zio_decisions.typesafe.Wire
import com.jamesward.zio_decisions.orchestration.AgentLoop.*
import com.jamesward.zio_decisions.orchestration.ParallelLoop.{Part, Plan, PlanRequest, Planner}
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object ParallelLoopSpec extends ZIOSpecDefault:

  private val edit = Tool("edit", "Edits a file.", Json.Obj(
    "type" -> Json.Str("object"),
    "properties" -> Json.Obj("who" -> Json.Obj("type" -> Json.Str("string"))),
    "required" -> Json.Arr(Json.Str("who")),
  ))

  /** Jev: splits when asked; in a loop, calls the tool once, then answers. */
  private val jev = Always(decisionAnswer)

  private def decisionAnswer(request: Wire.SystemOneRequest): Respond =
    val body = request.body.toString
    val questions = request.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).toList.flatMap(_.fields.map(_._1))
    if body.contains("\"split\"") then Respond(Map("split" -> Wire.Answer.Noul(0.9)), Wire.Usage(50, 1))
    // a relevance request: the first tool is relevant, the rest aren't
    else if !questions.contains("next_action") then
      Respond(questions.map(q => q -> Wire.Answer.Noul(if q == "tool_000" then 0.95 else 0.05)).toMap, Wire.Usage(80, 1))
    else
      val steps = request.body.asObject.flatMap(_.get("state")).flatMap(_.asObject).flatMap(_.get("steps")).flatMap(_.asArray)
      val id = if steps.forall(_.isEmpty) then "tool_000" else "end_answer"
      Respond(Map("next_action" -> Wire.Answer.Choice(id, Map(id -> 0.9), 0.9)), Wire.Usage(100, 5))

  /** Plans `parts`; writes each call's `who` as the loop its prompt names; replies with the prompt's first line. */
  private final class Model(parts: List[String], seen: Ref[Vector[ArgumentsRequest]], setup: String = "",
      plans: Option[Ref[Vector[PlanRequest]]] = None) extends Llm, Planner:
    def plan(request: PlanRequest): Task[LlmResult[Plan]] =
      ZIO.foreachDiscard(plans)(_.update(_ :+ request)).as(LlmResult(Plan(parts.map(p => Part(p, s"do $p")), setup), 30, 10))
    def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]] =
      val who =
        if request.prompt.contains("Before this request's work is planned") then "gather"
        else if request.prompt.contains("Write a first version of every part") then "draft"
        else if request.prompt.contains("for one shared cause") then "repair"
        else if request.prompt.contains("First, only the setup") then "setup"
        else if request.prompt.contains("Check them together") then "integration"
        else parts.find(p => request.prompt.contains(s"Yours is \"$p\"")).getOrElse("main")
      seen.update(_ :+ request).as(LlmResult(Json.Obj("who" -> Json.Str(who)), 10, 2))
    def reply(request: ReplyRequest): Task[LlmResult[String]] = ZIO.succeed(LlmResult(s"done: ${request.steps.size} steps", 20, 4))

  private val noDraft = ParallelLoop.Config(draftFirst = false, judgeRetry = false)

  def spec = suite("ParallelLoop")(
    test("parts run at the same time after the shared work; the log records every loop's calls; the join sees them all") {
      for
        mock    <- TypeSafeMock.tracked(jev)
        seen    <- Ref.make(Vector.empty[ArgumentsRequest])
        // each part's call waits until both parts are calling: this only finishes when they run at the same time
        calling <- Ref.make(0)
        both    <- Promise.make[Nothing, Unit]
        loops   <- Ref.make(Vector.empty[(String, Option[String])])
        tools    = new Tools:
                     def call(name: String, arguments: Json.Obj): Task[Json] =
                       val who = arguments.get("who").flatMap(_.asString).getOrElse("")
                       ParallelLoop.currentLoop.get.flatMap(l => loops.update(_ :+ (who -> l))) *>
                       ZIO.when(who.startsWith("part"))(
                         calling.updateAndGet(_ + 1).flatMap(n => both.succeed(()).when(n == 2)) *>
                           both.await.timeoutFail(RuntimeException("not concurrent"))(5.seconds))
                         .as(Json.Obj("edited" -> Json.Str(who)))
        plans   <- Ref.make(Vector.empty[PlanRequest])
        result  <- ParallelLoop.run("build it", List(edit), tools, Model(List("part A", "part B"), seen, plans = Some(plans)), noDraft).provideLayer(mock.decisions)
        llmSeen <- seen.get
        callers <- loops.get
        planned <- plans.get
      yield
        val loops = result.events.map(_.loop)
        val join = llmSeen.filter(_.prompt.contains("Check them together"))
        assertTrue(
          loops.head == "gather", loops.last == "integration", loops.toSet == Set("gather", "part A", "part B", "integration"),
          result.plan.map(_.parts.map(_.name)).contains(List("part A", "part B")),
          result.parts.map(_._1) == List("part A", "part B"), result.ending == Ending.Answered,
          // the join's context is the log's projection: the shared work and both parts' calls
          join.flatMap(_.context).exists(c => c.contains("[part A], working at the same time: edit") &&
            c.contains("[part B], working at the same time: edit") && c.contains("Gathered for the whole request")),
          // the planner was given what was gathered
          planned.size == 1, planned.head.gathered.exists(_.contains("gather")),
          // a part sees the shared work, not its own calls as another's
          llmSeen.filter(_.prompt.contains("Yours is \"part A\"")).flatMap(_.context).forall(!_.contains("[part A]")),
          llmSeen.filter(_.prompt.contains("Yours is \"part A\"")).flatMap(_.context).exists(_.contains("Gathered for the whole request")),
          result.steps.size == 4, result.reply == "done: 1 steps",
          // the tools know which loop each call is for
          callers.forall((who, loop) => loop.contains(who)),
          // gather 2, split, parts 2+2, join 2 decisions; the gathering's and the parts' calls, the plan and the reply
          result.decisions.size == 9, result.llmCalls == 1 + 4 + 1, result.llmInputTokens == 30L + 4 * 10 + 20,
        )
    } @@ TestAspect.withLiveClock,
    test("a part whose loop fails is that part's outcome: the others and the join go on, and the join is told") {
      val failing = AlwaysOr: request =>
        if request.body.toString.contains("Yours is \\\"part B\\\"") then
          Left(com.jamesward.zio_decisions.Decisions.Error.NotFound("boom"))
        else Right(decisionAnswer(request))
      for
        mock    <- TypeSafeMock.tracked(failing)
        seen    <- Ref.make(Vector.empty[ArgumentsRequest])
        tools    = new Tools:
                     def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.succeed(Json.Obj("ok" -> Json.Bool(true)))
        result  <- ParallelLoop.run("build it", List(edit), tools, Model(List("part A", "part B"), seen), noDraft).provideLayer(mock.decisions)
        llmSeen <- seen.get
      yield assertTrue(
        result.parts.map(p => p._1 -> p._2) == List("part A" -> Ending.Answered, "part B" -> Ending.Unable),
        result.ending == Ending.Answered,
        llmSeen.exists(r => r.prompt.contains("Check them together") && r.prompt.contains("\"part B\": its agent failed")),
      )
    },
    test("interrupting the run interrupts every part's work") {
      for
        mock    <- TypeSafeMock.tracked(jev)
        seen    <- Ref.make(Vector.empty[ArgumentsRequest])
        started <- Ref.make(0)
        stopped <- Ref.make(0)
        both    <- Promise.make[Nothing, Unit]
        tools    = new Tools:
                     def call(name: String, arguments: Json.Obj): Task[Json] =
                       val who = arguments.get("who").flatMap(_.asString).getOrElse("")
                       if !who.startsWith("part") then ZIO.succeed(Json.Obj())
                       else (started.updateAndGet(_ + 1).flatMap(n => both.succeed(()).when(n == 2)) *> ZIO.never)
                         .onInterrupt(stopped.update(_ + 1)).as(Json.Obj())
        fiber   <- ParallelLoop.run("build it", List(edit), tools, Model(List("part A", "part B"), seen), noDraft).provideLayer(mock.decisions).fork
        _       <- both.await
        _       <- fiber.interrupt
        n       <- stopped.get
      yield assertTrue(n == 2)
    } @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds),
    test("a loop's view of the log: shared work and other loops' calls, never its own; fitted, older reads go first") {
      def event(n: Int, loop: String, tool: String, text: String) =
        ParallelLoop.Event(n, loop, tool, Json.Obj("n" -> Json.Num(n)), Outcome.Succeeded(Json.Str(text)))
      val events = Vector(
        event(1, "gather", "read", "the spec " + "s" * 5000),
        event(2, "a", "read", "a read"), event(3, "b", "read", "b read " + "r" * 500), event(4, "b", "edit", "b edited"),
        event(5, "a", "edit", "a edited"), event(6, "b", "read", "b read again"),
      )
      val view = ParallelLoop.view(events, "a", Set("read"))
      val small = view.fit(4000).render
      assertTrue(
        !view.render.contains("[a]"), view.render.contains("[b]"), view.render.contains("the spec"),
        // under a tight budget the shared spec survives (to its floor), b's edit outlasts b's reads
        small.contains("the spec"), small.contains("b edited"), small.length <= 4000,
        view.fit(3600).render.contains("b edited") || !view.fit(3600).render.contains("b read again"),
      )
    },
    test("a plan's parts are uniquely named, with a task each, at most the maximum") {
      val plan = Plan(List(Part("x", "do x"), Part("x", "do x again"), Part("", "unnamed"), Part("y", " "), Part("z", "do z")), "")
      assertTrue(ParallelLoop.normalize(plan, 3).parts.map(_.name) == List("x", "x (2)", "part"))
    },
    test("time: with the tools' time simulated, the run takes shared + the longest part + the join, not the parts' sum") {
      // each part's call takes 10 s × its number, the shared and the join calls 5 s; Jev and the model take no time
      val parts = List("part 1", "part 2", "part 3", "part 4")
      for
        mock    <- TypeSafeMock.tracked(jev)
        seen    <- Ref.make(Vector.empty[ArgumentsRequest])
        tools    = new Tools:
                     def call(name: String, arguments: Json.Obj): Task[Json] =
                       val who = arguments.get("who").flatMap(_.asString).getOrElse("")
                       val seconds = who.stripPrefix("part ").toIntOption.fold(5)(_ * 10)
                       ZIO.sleep(seconds.seconds).as(Json.Obj())
        fiber   <- (ParallelLoop.run("build it", List(edit), tools, Model(parts, seen), noDraft).provideLayer(mock.decisions) <*> Clock.instant).fork
        _       <- TestClock.adjust(1.second).repeatN(200)
        joined  <- fiber.join
        (result, now) = joined
      yield
        // shared 5 + longest part 40 + join 5 = 50 s (not 5 + 10+20+30+40 + 5 = 110 s); the clock moved in 1 s steps
        val elapsed = now.getEpochSecond
        assertTrue(result.parts.size == 4, result.parts.forall(_._2 == Ending.Answered), elapsed >= 50, elapsed <= 52)
    },
    test("with many tools, each loop is offered those Jev thinks its task needs (and an option for the rest)") {
      val many = edit :: (1 to 15).map(i => Tool(s"other_$i", s"Does other thing $i.", Json.Obj("type" -> Json.Str("object")))).toList
      for
        mock     <- TypeSafeMock.tracked(jev)
        seen     <- Ref.make(Vector.empty[ArgumentsRequest])
        tools     = new Tools:
                      def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.succeed(Json.Obj())
        _        <- ParallelLoop.run("build it", many, tools, Model(List("part A", "part B"), seen), noDraft).provideLayer(mock.decisions)
        requests <- mock.requests
        decisions = requests.filter(_.body.toString.contains("next_action"))
        offered   = decisions.map(_.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).flatMap(_.get("next_action"))
                      .flatMap(_.asObject).flatMap(_.get("criteria")).flatMap(_.asObject).toList.flatMap(_.fields.map(_._1)))
      yield assertTrue(
        // a relevance request per loop: the gathering, two parts, integration
        requests.count(r => r.body.toString.contains("Could the agent need this tool")) == 4,
        decisions.nonEmpty,
        offered.forall(o => o.count(_.startsWith("tool_")) == AgentLoop.Strategy.MinRelevant && o.contains("more_tools")),
      )
    },
    test("a plan's setup is done once, after the plan and before the parts, and every part sees it") {
      for
        mock    <- TypeSafeMock.tracked(jev)
        seen    <- Ref.make(Vector.empty[ArgumentsRequest])
        tools    = new Tools:
                     def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.succeed(Json.Obj("done" -> arguments.get("who").getOrElse(Json.Null)))
        result  <- ParallelLoop.run("build it", List(edit), tools, Model(List("part A", "part B"), seen, setup = "add the helper"), noDraft)
                     .provideLayer(mock.decisions)
        llmSeen <- seen.get
      yield assertTrue(
        result.events.map(_.loop).take(2) == Vector("gather", "setup"),
        llmSeen.filter(_.prompt.contains("Yours is")).flatMap(_.context).forall(_.contains("Setup for every part")),
      )
    },
    test("a part that ends unable gets another round, from its first attempt's calls, and its outcome is the second's") {
      // part B's first loop is unable at once; its second round calls the tool and answers
      val unableOnce = Always: request =>
        val body = request.body.toString
        if body.contains("Yours is \\\"part B\\\"") && !body.contains("attempted once already") &&
            body.contains("next_action") then
          Respond(Map("next_action" -> Wire.Answer.Choice("end_unable", Map("end_unable" -> 0.9), 0.9)), Wire.Usage(100, 5))
        else decisionAnswer(request)
      for
        mock    <- TypeSafeMock.tracked(unableOnce)
        seen    <- Ref.make(Vector.empty[ArgumentsRequest])
        tools    = new Tools:
                     def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.succeed(Json.Obj("ok" -> Json.Bool(true)))
        result  <- ParallelLoop.run("build it", List(edit), tools, Model(List("part A", "part B"), seen), noDraft).provideLayer(mock.decisions)
      yield assertTrue(
        result.parts.map(p => p._1 -> p._2) == List("part A" -> Ending.Answered, "part B" -> Ending.Answered),
        result.events.map(_.loop).contains("part B (again)"),
        !result.events.map(_.loop).contains("part A (again)"),
      )
    },
    test("the gathering is offered only the tools that read") {
      val read = Tool("read", "Reads a file.", Json.Obj("type" -> Json.Str("object")), annotations = Some(Json.Obj("readOnlyHint" -> Json.Bool(true))))
      for
        mock     <- TypeSafeMock.tracked(jev)
        seen     <- Ref.make(Vector.empty[ArgumentsRequest])
        tools     = new Tools:
                      def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.succeed(Json.Obj())
        result   <- ParallelLoop.run("build it", List(edit, read), tools, Model(List("part A", "part B"), seen), noDraft).provideLayer(mock.decisions)
      yield assertTrue(result.events.filter(_.loop == "gather").forall(_.tool == "read"), result.events.exists(_.loop == "gather"))
    },
    test("a part stopped through the process tree ends stopped, isn't tried again, and the others and the join go on") {
      for
        mock    <- TypeSafeMock.tracked(jev)
        seen    <- Ref.make(Vector.empty[ArgumentsRequest])
        tree    <- ProcessTree.make
        tools    = new Tools:
                     def call(name: String, arguments: Json.Obj): Task[Json] =
                       if arguments.get("who").flatMap(_.asString).contains("part B") then ZIO.never else ZIO.succeed(Json.Obj())
        fiber   <- tree.child(None, ProcessTree.Kind.Task, "task")(_ =>
                     ParallelLoop.run("build it", List(edit), tools, Model(List("part A", "part B"), seen), noDraft).provideLayer(mock.decisions)).fork
        loop    <- tree.running.repeatUntil(_.exists(n => n.kind == ProcessTree.Kind.Loop && n.name == "part B"))
                     .map(_.find(_.name == "part B").get)
        _       <- tree.stop(loop.id, "the user stopped it")
        result  <- fiber.join
        llmSeen <- seen.get
      yield assertTrue(
        result.parts.find(_._1 == "part B").exists(_._2 == Ending.Unable),
        !result.events.exists(_.loop == "part B (again)"),
        llmSeen.exists(r => r.prompt.contains("Check them together") && r.prompt.contains("stopped (the user stopped it)")),
        result.ending == Ending.Answered,
      )
    } @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds),
    test("draft first: one loop writes every part and runs the tests; only the parts Jev judges unfinished are forked") {
      // Jev: the draft did part A (its test passed), not part B
      val judging = Always: request =>
        val qs = request.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).toList.flatMap(_.fields.map(_._1))
        if qs.exists(_.startsWith("part_")) then
          Respond(Map("part_000" -> Wire.Answer.Noul(0.95), "part_001" -> Wire.Answer.Noul(0.1)), Wire.Usage(30, 1))
        else decisionAnswer(request)
      for
        mock    <- TypeSafeMock.tracked(judging)
        seen    <- Ref.make(Vector.empty[ArgumentsRequest])
        tools    = new Tools:
                     def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.succeed(Json.Str("part A PASSED, part B FAILED"))
        // the one tool runs the tests, so the draft's call is its test run
        result  <- ParallelLoop.run("build it", List(edit.copy(name = "run_tests")), tools, Model(List("part A", "part B"), seen))
                     .provideLayer(mock.decisions)
        loops    = result.events.map(_.loop)
      yield assertTrue(
        loops.contains("draft"), loops.contains("part B"), !loops.contains("part A"),
        // the draft ran the tests once
        result.events.count(e => e.loop == "draft" && e.outcome.isInstanceOf[Outcome.Succeeded]) == 1,
        result.parts.map(_._1) == List("part A", "part B"), result.parts.forall(_._2 == Ending.Answered),
      )
    },
    test("auto: Jev's no to several parts is one loop from the start; a yes is the parallel run") {
      def decisionSays(p: Double) = Always: request =>
        if request.body.toString.contains("\"several_parts\"") then Respond(Map("several_parts" -> Wire.Answer.Noul(p)), Wire.Usage(20, 1))
        else decisionAnswer(request)
      val tools = new Tools:
        def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.succeed(Json.Obj())
      for
        one     <- TypeSafeMock.tracked(decisionSays(0.1))
        many    <- TypeSafeMock.tracked(decisionSays(0.9))
        seen    <- Ref.make(Vector.empty[ArgumentsRequest])
        single  <- ParallelLoop.auto("fix it", List(edit), tools, Model(List("part A", "part B"), seen), noDraft).provideLayer(one.decisions)
        split   <- ParallelLoop.auto("build it", List(edit), tools, Model(List("part A", "part B"), seen), noDraft).provideLayer(many.decisions)
      yield assertTrue(
        single.events.map(_.loop) == Vector("main"), single.plan.isEmpty, single.decisions.head.choice == "one",
        split.events.map(_.loop).toSet == Set("gather", "part A", "part B", "integration"), split.decisions.head.choice == "several",
      )
    },
    test("a test run's digest keeps each test's result line and the summary, not the stack traces") {
      val output = "> Task :test\n\nMainTest > scenario1() PASSED\n\nMainTest > scenario2() FAILED\n" +
        (1 to 400).map(i => s"        at some.Frame$i(Frame.java:$i)").mkString("\n") + "\n\nMainTest > scenario3() PASSED\n" +
        (1 to 400).map(i => s"        at other.Frame$i(Frame.java:$i)").mkString("\n") + "\n3 tests completed, 1 failed\nBUILD FAILED in 9s"
      val d = ParallelLoop.digest(output.replace("scenario2() FAILED\n", "scenario2() FAILED\n    java.util.concurrent.TimeoutException: scenario2() timed out after 15 seconds\n"), 3000)
      assertTrue(d.contains("scenario1() PASSED"), d.contains("scenario2() FAILED"), d.contains("scenario3() PASSED"),
        d.contains("scenario2() timed out after 15 seconds"),
        d.contains("BUILD FAILED"), d.length <= 3100, !d.contains("Frame17("))
    },
    test("a draft whose parts fail for one shared cause: one repair loop fixes it, and the parts it then passes aren't forked") {
      // the draft passes nothing; Jev sees one shared cause; after the repair, both parts pass
      val judging = Always: request =>
        val body = request.body.toString
        val qs = request.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).toList.flatMap(_.fields.map(_._1))
        if qs.contains("shared_cause") then Respond(Map("shared_cause" -> Wire.Answer.Noul(0.9)), Wire.Usage(30, 1))
        else if qs.exists(_.startsWith("part_")) then
          val passed = body.contains("both pass")
          Respond(Map("part_000" -> Wire.Answer.Noul(if passed then 0.95 else 0.05), "part_001" -> Wire.Answer.Noul(if passed then 0.95 else 0.05)), Wire.Usage(30, 1))
        else decisionAnswer(request)
      for
        mock    <- TypeSafeMock.tracked(judging)
        seen    <- Ref.make(Vector.empty[ArgumentsRequest])
        runs    <- Ref.make(0)
        tools    = new Tools:
                     def call(name: String, arguments: Json.Obj): Task[Json] =
                       runs.updateAndGet(_ + 1).map(n => Json.Str(if arguments.get("who").flatMap(_.asString).contains("repair") then "both pass" else "both time out"))
        result  <- ParallelLoop.run("build it", List(edit.copy(name = "run_tests")), tools, Model(List("part A", "part B"), seen))
                     .provideLayer(mock.decisions)
        loops    = result.events.map(_.loop)
      yield assertTrue(
        loops.contains("draft"), loops.contains("repair"), !loops.contains("part A"), !loops.contains("part B"),
        result.parts.map(_._1) == List("part A", "part B"),
      )
    },
    test("each part gets the gathered paragraphs Jev judges it needs, verbatim, in its context") {
      val spec = "Intro paragraph that is long enough to stand on its own as a paragraph of the specification text here, " +
        "with a second sentence so that it isn't joined to the paragraph after it.\n\n" +
        "1. Part A: do the A thing with GET /a, and the winner returns right. " + "a" * 200 + "\n" +
        "2. Part B: do the B thing with GET /b, and the winner returns right. " + "b" * 200
      val read = Tool("read", "Reads a file.", Json.Obj("type" -> Json.Str("object")), annotations = Some(Json.Obj("readOnlyHint" -> Json.Bool(true))))
      // Jev: a paragraph is needed by the part its text names
      val picking = Always: request =>
        val qs = request.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).toList.flatMap(_.fields)
        if qs.exists(_._1.startsWith("para_")) then
          val part = request.body.asObject.flatMap(_.get("state")).flatMap(_.asObject).flatMap(_.get("part")).flatMap(_.asString).getOrElse("")
          Respond(qs.map((id, q) => id -> Wire.Answer.Noul(if q.toString.contains(part.capitalize) then 0.9 else 0.1)).toMap, Wire.Usage(30, 1))
        else decisionAnswer(request)
      for
        mock    <- TypeSafeMock.tracked(picking)
        seen    <- Ref.make(Vector.empty[ArgumentsRequest])
        tools    = new Tools:
                     def call(name: String, arguments: Json.Obj): Task[Json] =
                       ZIO.succeed(if name == "read" then Json.Obj("text" -> Json.Str(spec)) else Json.Obj())
        // the parts' first tool writes arguments (the gathering is offered only the read)
        _       <- ParallelLoop.run("build it", List(edit, read), tools, Model(List("part A", "part B"), seen), noDraft).provideLayer(mock.decisions)
        llmSeen <- seen.get
        contexts = llmSeen.filter(_.prompt.contains("Yours is \"part A\"")).flatMap(_.context)
      yield assertTrue(
        ParallelLoop.paragraphs(Vector(ParallelLoop.Event(1, "gather", "read", Json.Obj(), Outcome.Succeeded(Json.Obj("text" -> Json.Str(spec)))))).size == 3,
        contexts.nonEmpty,
        contexts.forall(c => c.contains("The specification of your part, verbatim") && c.contains("1. Part A: do the A thing")),
      )
    },
    test("a part's round longer than its limit ends, is tried once more, and the others and the join go on") {
      for
        mock    <- TypeSafeMock.tracked(jev)
        seen    <- Ref.make(Vector.empty[ArgumentsRequest])
        tools    = new Tools:
                     def call(name: String, arguments: Json.Obj): Task[Json] =
                       if arguments.get("who").flatMap(_.asString).exists(_.startsWith("part B")) then ZIO.never else ZIO.succeed(Json.Obj())
        fiber   <- ParallelLoop.run("build it", List(edit), tools, Model(List("part A", "part B"), seen),
                     noDraft.copy(partTimeout = Some(1.minute))).provideLayer(mock.decisions).fork
        _       <- TestClock.adjust(10.seconds).repeatN(20)
        result  <- fiber.join
      yield assertTrue(
        result.parts.map(p => p._1 -> p._2) == List("part A" -> Ending.Answered, "part B" -> Ending.Unable),
        result.ending == Ending.Answered,
      )
    },
    test("judgeRetry: a part that ended unable gets no other round when Jev judges one unlikely to help") {
      val unable = Always: request =>
        val body = request.body.toString
        val qs = request.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).toList.flatMap(_.fields.map(_._1))
        if qs.contains("another_round") then Respond(Map("another_round" -> Wire.Answer.Noul(0.1)), Wire.Usage(10, 1))
        else if body.contains("Yours is \\\"part B\\\"") && qs.contains("next_action") then
          Respond(Map("next_action" -> Wire.Answer.Choice("end_unable", Map("end_unable" -> 0.9), 0.9)), Wire.Usage(10, 1))
        else decisionAnswer(request)
      for
        mock    <- TypeSafeMock.tracked(unable)
        seen    <- Ref.make(Vector.empty[ArgumentsRequest])
        tools    = new Tools:
                     def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.succeed(Json.Obj())
        result  <- ParallelLoop.run("build it", List(edit), tools, Model(List("part A", "part B"), seen), noDraft.copy(judgeRetry = true))
                     .provideLayer(mock.decisions)
        asked   <- mock.requests
      yield assertTrue(
        !result.events.exists(_.loop == "part B (again)"),
        asked.exists(_.body.toString.contains("another_round")),
        result.parts.find(_._1 == "part B").exists(_._2 == Ending.Unable),
      )
    },
    test("progress: the plan's entries are reported, in order, each state change, ending all completed") {
      for
        mock    <- TypeSafeMock.tracked(jev)
        seen    <- Ref.make(Vector.empty[ArgumentsRequest])
        reports <- Ref.make(Vector.empty[List[(String, ParallelLoop.Progress)]])
        tools    = new Tools:
                     def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.succeed(Json.Obj())
        _       <- ParallelLoop.run("build it", List(edit), tools, Model(List("part A", "part B"), seen),
                     noDraft.copy(progress = es => reports.update(_ :+ es))).provideLayer(mock.decisions)
        all     <- reports.get
      yield assertTrue(
        all.head.map(_._1).head == "Read what the request needs",
        all.head.map(_._1).exists(_.startsWith("part A:")), all.head.map(_._1).last.startsWith("Check the parts"),
        all.exists(_.exists((n, st) => n.startsWith("part B:") && st == ParallelLoop.Progress.InProgress)),
        all.last.forall(_._2 == ParallelLoop.Progress.Completed),
      )
    },
    test("a plan of one part runs as one loop") {
      for
        mock   <- TypeSafeMock.tracked(jev)
        seen   <- Ref.make(Vector.empty[ArgumentsRequest])
        tools   = new Tools:
                    def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.succeed(Json.Obj())
        result <- ParallelLoop.run("fix it", List(edit), tools, Model(List("only"), seen), noDraft).provideLayer(mock.decisions)
        asked  <- mock.requests
      // the gathering, then one loop that sees what it gathered
      yield assertTrue(result.events.map(_.loop) == Vector("gather", "main"), !asked.exists(_.body.toString.contains("\"split\"")))
    },
  )
