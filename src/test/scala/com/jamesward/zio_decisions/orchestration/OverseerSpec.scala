package com.jamesward.zio_decisions.orchestration

import com.jamesward.zio_decisions.TypeSafeMock
import com.jamesward.zio_decisions.TypeSafeMock.MockBehavior.Respond
import com.jamesward.zio_decisions.typesafe.Wire
import com.jamesward.zio_decisions.orchestration.ProcessTree.Kind
import com.jamesward.zio_decisions.orchestration.Overseer.Verdict
import zio.*
import zio.test.*

object OverseerSpec extends ZIOSpecDefault:
  def spec = suite("Overseer")(
    test("a stalled call's history is the ended calls like it; Jev's choice per call is its verdict") {
      for
        tree    <- ProcessTree.make
        task    <- tree.child(None, Kind.Task, "task-1", Some("fix the bug"))(id =>
                     ZIO.foreachDiscard(1 to 3)(_ => ProcessTree.run(Kind.Call, "execute_terminal_command", Some("./gradlew test --tests A"))(ZIO.sleep(15.seconds))) *>
                       (ProcessTree.run(Kind.Call, "execute_terminal_command", Some("./gradlew test --tests A"))(ZIO.never) <&>
                         ProcessTree.run(Kind.Call, "read_file", Some("Main.java"))(ZIO.never))).fork
        _       <- TestClock.adjust(15.seconds).repeatN(2)
        _       <- tree.running.repeatUntil(_.count(_.kind == Kind.Call) == 2)
        _       <- TestClock.adjust(200.seconds)
        stalled <- Overseer.stalled(tree, 60.seconds)
        mock    <- TypeSafeMock.tracked(Respond(Map(
                     "call_000" -> Wire.Answer.Choice("stop", Map("stop" -> 0.9, "wait" -> 0.1), 0.9),
                     "call_001" -> Wire.Answer.Choice("wait", Map("stop" -> 0.2, "wait" -> 0.8), 0.8),
                   ), Wire.Usage(50, 2)))
        verdicts <- Overseer.judge(stalled).provideLayer(mock.decisions)
        asked    <- mock.requests
        _        <- task.interrupt
      yield
        val gradle = stalled.find(_.node.name == "execute_terminal_command").get
        assertTrue(
          stalled.size == 2,
          gradle.similar == List(15L, 15L, 15L), gradle.task.contains("fix the bug"), gradle.runningSeconds == 200,
          stalled.find(_.node.name == "read_file").exists(_.similar.isEmpty),
          verdicts.map(_.verdict) == List(Verdict.Stop, Verdict.Wait),
          asked.size == 1, asked.head.body.toString.contains("earlierRunsOfThisCallSeconds"),
        )
    },
    test("nothing stalled: no Jev request") {
      for
        mock <- TypeSafeMock.tracked()
        out  <- Overseer.judge(Nil).provideLayer(mock.decisions)
        n    <- mock.requestCount
      yield assertTrue(out.isEmpty, n == 0)
    },
  )
