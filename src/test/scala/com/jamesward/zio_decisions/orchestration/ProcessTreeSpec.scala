package com.jamesward.zio_decisions.orchestration

import com.jamesward.zio_decisions.TypeSafeMock
import com.jamesward.zio_decisions.TypeSafeMock.MockBehavior.Respond
import com.jamesward.zio_decisions.typesafe.Wire
import com.jamesward.zio_decisions.orchestration.AgentLoop.*
import com.jamesward.zio_decisions.orchestration.ProcessTree.{Kind, State}
import zio.*
import zio.json.ast.Json
import zio.test.*

object ProcessTreeSpec extends ZIOSpecDefault:

  private def choose(id: String) = Respond(Map("next_action" -> Wire.Answer.Choice(id, Map(id -> 0.9), 0.9)), Wire.Usage(10, 1))

  private val tool = Tool("hang", "Hangs.", Json.Obj("type" -> Json.Str("object")))

  private final class Llm0 extends Llm:
    def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]] = ZIO.succeed(LlmResult(Json.Obj(), 1, 1))
    def reply(request: ReplyRequest): Task[LlmResult[String]] =
      ZIO.succeed(LlmResult(request.steps.map(_.outcome.toString).mkString(";"), 1, 1))

  /** Waits until `tree` has a running node of `kind`. */
  private def await(tree: ProcessTree, kind: Kind) =
    tree.running.repeatUntil(_.exists(_.kind == kind)).map(_.find(_.kind == kind).get)

  def spec = (suite("ProcessTree")(
    test("goals: a node's goal is set as it starts, changed and removed while it runs, each change kept with why; children inherit it") {
      for
        tree    <- ProcessTree.make
        gate    <- Promise.make[Nothing, Unit]
        seen    <- Promise.make[Nothing, (Option[String], Option[String])]
        fiber   <- tree.child(None, ProcessTree.Kind.Task, "task-1", Some("fix it"), goal = Some("fix the checkout bug"))(_ =>
                     ProcessTree.run(ProcessTree.Kind.Loop, "part A", goal = Some("part A: the discount"))(
                       ProcessTree.goal.zip(ProcessTree.run(ProcessTree.Kind.Call, "read_file")(ProcessTree.goal)).flatMap(seen.succeed) *> gate.await)).fork
        inner   <- seen.await
        task    <- tree.snapshot.map(_.find(_.name == "task-1").get)
        changed <- tree.setGoal(task.id, "fix the checkout bug, then add a test", "the user steered it")
        now     <- Clock.instant
        shown   <- tree.snapshot.map(ProcessTree.render(_, now))
        removed <- tree.removeGoal(task.id, "the user asked to stop")
        after   <- tree.snapshot.map(_.find(_.name == "task-1").get)
        _       <- gate.succeed(())
        _       <- fiber.join
        ended   <- tree.setGoal(task.id, "too late", "x")
      yield assertTrue(
        // the part's sub-agent has its own goal; its calls work toward it
        inner == (Some("part A: the discount"), Some("part A: the discount")),
        task.goal.contains("fix the checkout bug"), changed, removed,
        shown.contains("goal now: fix the checkout bug, then add a test (the user steered it)"),
        after.goal.isEmpty, after.goals.map(_.why) == Vector("the work's start", "the user steered it", "the user asked to stop"),
        !ended,
      )
    },
    test("nodes nest by the fiber they run in, and record how each ended") {
      for
        tree <- ProcessTree.make
        _    <- tree.child(None, Kind.Task, "task")(_ =>
                  ProcessTree.run(Kind.Loop, "loop")(
                    ProcessTree.run(Kind.Call, "ok")(ZIO.unit) *>
                      ProcessTree.run(Kind.Call, "bad")(ZIO.fail(RuntimeException("boom"))).ignore))
        all  <- tree.snapshot
      yield
        val byName = all.map(n => n.name -> n).toMap
        assertTrue(
          byName("loop").parent.contains(byName("task").id), byName("ok").parent.contains(byName("loop").id),
          byName("ok").state == State.Done, byName("bad").state == State.Failed, byName("bad").outcome.contains("boom"),
          byName("task").state == State.Done,
        )
    },
    test("a hung tool call is seen as stalled and, stopped, is a failed step: the loop goes on and ends") {
      for
        tree   <- ProcessTree.make
        jev    <- TypeSafeMock.tracked(choose("tool_000"), choose("end_answer"))
        hang    = new Tools:
                    def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.never
        fiber  <- tree.child(None, Kind.Task, "task")(_ => AgentLoop.run("go", List(tool), hang, Llm0()).provideLayer(jev.decisions)).fork
        call   <- await(tree, Kind.Call)
        _      <- TestClock.adjust(2.minutes)
        stuck  <- tree.stalled(1.minute)
        text   <- Clock.instant.flatMap(now => tree.snapshot.map(ProcessTree.render(_, now)))
        _      <- tree.stop(call.id, "hung")
        result <- fiber.join
        after  <- tree.snapshot
      yield assertTrue(
        stuck.map(_.id) == Vector(call.id),
        text.contains("call hang: running 120s, nothing for 120s"),
        result.ending == Ending.Answered,
        result.steps.head.outcome.toString.contains("stopped: hung"),
        after.find(_.id == call.id).exists(_.state == State.Stopped),
      )
    },
    test("stopping a task stops everything below it") {
      for
        tree  <- ProcessTree.make
        down  <- Ref.make(0)
        fiber <- tree.child(None, Kind.Task, "task")(_ =>
                   ZIO.foreachPar(1 to 3)(i => ProcessTree.run(Kind.Call, s"c$i")(ZIO.never.onInterrupt(down.update(_ + 1))))).fork
        _     <- tree.running.repeatUntil(_.count(_.kind == Kind.Call) == 3)
        task  <- tree.running.map(_.find(_.kind == Kind.Task).get)
        _     <- tree.stop(task.id, "user")
        exit  <- fiber.await
        n     <- down.get
        all   <- tree.snapshot
      yield assertTrue(
        n == 3, exit.isFailure,
        all.forall(_.state == State.Stopped),
      )
    },
    test("watchers are told when a node starts, and when it ends: no polling") {
      for
        tree    <- ProcessTree.make
        seen    <- ZIO.scoped:
                     for
                       starts <- tree.started
                       gate   <- Promise.make[Nothing, Unit]
                       fiber  <- tree.child(None, Kind.Call, "build")(_ => gate.await).fork
                       first  <- starts.take
                       ending <- tree.awaitEnd(first.id).fork
                       _      <- gate.succeed(())
                       _      <- fiber.join
                       ended  <- ending.join
                     yield (first, ended)
      yield assertTrue(seen._1.name == "build", seen._2.exists(_.state == State.Done))
    },
    test("render shows the tree indented, with what's running and how things ended") {
      val now = java.time.Instant.parse("2026-01-01T00:10:00Z")
      def at(m: Int) = java.time.Instant.parse(f"2026-01-01T00:$m%02d:00Z")
      val nodes = Vector(
        ProcessTree.Node(ProcessTree.Id(1), None, Kind.Task, "task-1", Some("fix the bug"), State.Running, at(0), at(9), None, None),
        ProcessTree.Node(ProcessTree.Id(2), Some(ProcessTree.Id(1)), Kind.Call, "read_file", None, State.Done, at(1), at(1), Some(at(1)), None),
        ProcessTree.Node(ProcessTree.Id(3), Some(ProcessTree.Id(1)), Kind.Call, "build", None, State.Running, at(2), at(2), None, None),
      )
      val text = ProcessTree.render(nodes, now)
      assertTrue(text == "[1] task task-1: running 600s, nothing for 60s — fix the bug\n  [2] call read_file: done after 0.0s\n  [3] call build: running 480s, nothing for 480s")
    },
    test("ended nodes are pruned to the last ones kept, never an ancestor of a running node") {
      for
        tree <- ProcessTree.make
        _    <- tree.child(None, Kind.Task, "long")(_ =>
                  ZIO.foreachDiscard(1 to ProcessTree.Keep + 50)(i => ProcessTree.run(Kind.Call, s"c$i")(ZIO.unit)) *>
                    tree.snapshot.map(_.count(_.kind == Kind.Call)).flatMap(n => ZIO.succeed(n)))
        all  <- tree.snapshot
      yield assertTrue(all.size <= ProcessTree.Keep, all.exists(_.name == s"c${ProcessTree.Keep + 50}"))
    },
  ) @@ TestAspect.timeout(60.seconds))
