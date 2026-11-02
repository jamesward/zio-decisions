package com.jamesward.zio_decisions.orchestration

import com.jamesward.zio_decisions.TypeSafeMock
import com.jamesward.zio_decisions.TypeSafeMock.MockBehavior.Respond
import com.jamesward.zio_decisions.typesafe.Wire
import com.jamesward.zio_decisions.orchestration.AgentLoop.*
import zio.*
import zio.json.ast.Json
import zio.test.*

object NoticesSpec extends ZIOSpecDefault:

  private val schema = Json.Obj("type" -> Json.Str("object"), "properties" -> Json.Obj("path" -> Json.Obj("type" -> Json.Str("string"))))
  private val readOnly = Some(Json.Obj("readOnlyHint" -> Json.Bool(true)))
  private val read = Tool("read_file", "Reads a file.", schema, annotations = readOnly)
  private val write = Tool("write_file", "Writes a file.", schema)

  /** The decision model: the first decision calls the first tool, the next ends; each notice judged `p`. */
  private def model(p: Double) = TypeSafeMock.MockBehavior.Always: request =>
    val body = request.body.toString
    if body.contains("\"judgment\"") then Respond(Map("judgment" -> Wire.Answer.Noul(p)))
    else
      val next = if body.contains("\"step\":1") then "end_answer" else "tool_000"
      Respond(Map("next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9)))

  private val llm = new Llm:
    def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]] = ZIO.succeed(LlmResult(Json.Obj("path" -> Json.Str("A.scala"))))
    def reply(request: ReplyRequest): Task[LlmResult[String]] = ZIO.succeed(LlmResult("done"))

  /**
   * A tool whose call takes an interest in `A.scala`, then has another loop (`b`) publish an edit of it (and things this
   * loop shouldn't hear: its own notice, one about another file), then takes `after` to end.
   */
  private def tools(bus: Notices.Bus, after: UIO[Unit]) = new Tools:
    def call(name: String, arguments: Json.Obj): Task[Json] =
      Notices.interest(Set("A.scala")) *>
        bus.publish("loop-b", "edit", Set("A.scala"), "loop-b replaced A.scala's discount with 15%") *>
        bus.publish("loop-x", "edit", Set("B.scala"), "UNRELATED change to B.scala") *>
        bus.publish("loop-a", "edit", Set("A.scala"), "OWN change to A.scala") *>
        after.as(Json.Str("text"))

  private def run(policy: Notices.Policy, p: Double, tool: Tool, after: UIO[Unit]) =
    for
      jev    <- TypeSafeMock.tracked(model(p))
      bus    <- Notices.Bus.make
      result <- Notices.bus.locally(Some(bus))(ParallelLoop.currentLoop.locally(Some("loop-a"))(
                  AgentLoop.run("change the discount", List(tool), tools(bus, after), llm, AgentLoop.Config(maxSteps = 4, notices = policy))
                    .provideLayer(jev.decisions)))
      reqs   <- jev.requests.map(_.map(_.body.toString))
    yield (result, reqs)

  def spec = suite("notices")(
    test("note: another loop's change of a file this one read is in its next decision; its own and unrelated ones aren't") {
      for
        // the first two notices are its own (from loop-a); a notice from another loop
        jev    <- TypeSafeMock.tracked(model(0.0))
        bus    <- Notices.Bus.make
        other   = new Tools:
                    def call(name: String, arguments: Json.Obj): Task[Json] =
                      Notices.interest(Set("A.scala")) *>
                        bus.publish("loop-b", "edit", Set("A.scala"), "loop-b replaced A.scala's discount with 15%") *>
                        bus.publish("loop-x", "edit", Set("B.scala"), "UNRELATED change to B.scala") *>
                        bus.publish("loop-a", "edit", Set("A.scala"), "OWN change to A.scala") *>
                        ZIO.sleep(200.millis).as(Json.Str("text"))
        _      <- Notices.bus.locally(Some(bus))(ParallelLoop.currentLoop.locally(Some("loop-a"))(
                    AgentLoop.run("change the discount", List(read), other, llm, AgentLoop.Config(maxSteps = 4, notices = Notices.Policy.Note))
                      .provideLayer(jev.decisions)))
        reqs   <- jev.requests.map(_.map(_.body.toString))
      yield
        val last = reqs.filterNot(_.contains("\"judgment\"")).last
        assertTrue(
          last.contains("Meanwhile, another task (edit)"), last.contains("replaced A.scala's discount with 15%"),
          !last.contains("UNRELATED"), !last.contains("OWN change"),
          // note: nothing judged
          !reqs.exists(_.contains("\"judgment\"")),
        )
    } @@ TestAspect.withLiveClock,
    test("judge: a change the decision model judges makes the work out of date is marked so, and the work isn't stopped") {
      for
        (result, reqs) <- run(Notices.Policy.Judge, 0.9, read, ZIO.sleep(300.millis))
      yield
        val last = reqs.filterNot(_.contains("\"judgment\"")).last
        assertTrue(
          reqs.exists(_.contains("\"judgment\"")),
          last.contains("Another task just changed what your work depends on"),
          result.steps.head.outcome.isInstanceOf[Outcome.Succeeded],
        )
    } @@ TestAspect.withLiveClock,
    test("interrupt: such a change stops the call in flight, a failed step saying why; the next decision sees it") {
      for
        (result, reqs) <- run(Notices.Policy.Interrupt, 0.9, read, ZIO.never)
      yield assertTrue(
        result.steps.head.outcome match
          case Outcome.Failed(e) => e.contains(Notices.OutOfDate) && e.contains("another task's change made this out of date")
          case _                 => false,
        reqs.filterNot(_.contains("\"judgment\"")).last.contains("Another task just changed what your work depends on"),
      )
    } @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds),
    test("interrupt: a change judged not to matter doesn't stop the call; an edit is never stopped") {
      for
        (judgedNot, _) <- run(Notices.Policy.Interrupt, 0.1, read, ZIO.sleep(300.millis))
        (edit, _)      <- run(Notices.Policy.Interrupt, 0.9, write, ZIO.sleep(300.millis))
      yield assertTrue(
        judgedNot.steps.head.outcome.isInstanceOf[Outcome.Succeeded],
        edit.steps.head.outcome.isInstanceOf[Outcome.Succeeded],
      )
    } @@ TestAspect.withLiveClock,
    test("a call that failed as out of date (another loop's change) can be made again with the same arguments") {
      val twice = TypeSafeMock.MockBehavior.Always: request =>
        val body = request.body.toString
        val next = if body.contains("\"step\":2") then "end_answer" else "tool_000"
        Respond(Map("next_action" -> Wire.Answer.Choice(next, Map(next -> 0.9), 0.9)))
      for
        jev    <- TypeSafeMock.tracked(twice)
        calls  <- Ref.make(0)
        tools   = new Tools:
                    def call(name: String, arguments: Json.Obj): Task[Json] =
                      calls.updateAndGet(_ + 1).flatMap(n =>
                        if n == 1 then ZIO.fail(RuntimeException(s"${Notices.OutOfDate}not edited, A.scala changed since you last read it"))
                        else ZIO.succeed(Json.Str("written")))
        result <- AgentLoop.run("change A", List(write), tools, llm, AgentLoop.Config(maxSteps = 4)).provideLayer(jev.decisions)
      yield assertTrue(result.steps.size == 2, result.steps(1).outcome == Outcome.Succeeded(Json.Str("written")))
    },
    test("off, or no bus: no inbox, nothing heard") {
      for
        jev  <- TypeSafeMock.tracked(model(0.9))
        bus  <- Notices.Bus.make
        _    <- AgentLoop.run("x", List(read), tools(bus, ZIO.unit), llm, AgentLoop.Config(maxSteps = 4, notices = Notices.Policy.Interrupt))
                  .provideLayer(jev.decisions)
        reqs <- jev.requests.map(_.map(_.body.toString))
      yield assertTrue(!reqs.exists(r => r.contains("\"judgment\"") || r.contains("another task")))
    } @@ TestAspect.withLiveClock,
  )
