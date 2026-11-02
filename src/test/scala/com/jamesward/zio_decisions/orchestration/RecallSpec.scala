package com.jamesward.zio_decisions.orchestration

import com.jamesward.zio_decisions.TypeSafeMock
import com.jamesward.zio_decisions.TypeSafeMock.MockBehavior.Respond
import com.jamesward.zio_decisions.typesafe.Wire
import com.jamesward.zio_decisions.orchestration.Recall.Earlier
import zio.*
import zio.test.*

object RecallSpec extends ZIOSpecDefault:
  private def task(n: Int) = Earlier(s"request $n", Some(s"reply $n"), "answered", 10L - n, sameSession = n % 2 == 0)

  def spec = suite("Recall")(
    test("one question per earlier task; those Jev judges relevant are recalled") {
      for
        mock     <- TypeSafeMock.tracked(Respond(Map(
                      "task_000" -> Wire.Answer.Noul(0.1), "task_001" -> Wire.Answer.Noul(0.9), "task_002" -> Wire.Answer.Noul(0.5))))
        recalled <- Recall.recall("now add tests for it", List(task(0), task(1), task(2))).provideLayer(mock.decisions)
        asked    <- mock.requests
      yield assertTrue(recalled == List(1, 2), asked.size == 1, asked.head.body.toString.contains("request 1"))
    },
    test("no earlier work: Jev isn't asked; only the newest candidates are asked about") {
      for
        none     <- TypeSafeMock.tracked(Respond(Map.empty))
        nothing  <- Recall.recall("hi", Nil).provideLayer(none.decisions)
        asked    <- none.requests
        many     <- TypeSafeMock.tracked(Respond((10 until 30).map(i => f"task_$i%03d" -> Wire.Answer.Noul(if i == 29 then 0.9 else 0.1)).toMap))
        recalled <- Recall.recall("go", (0 until 30).toList.map(task)).provideLayer(many.decisions)
        sent     <- many.requests
      yield assertTrue(nothing.isEmpty, asked.isEmpty, recalled == List(29),
        !sent.head.body.toString.contains("task_009"), sent.head.body.toString.contains("request 10"))
    },
  )
