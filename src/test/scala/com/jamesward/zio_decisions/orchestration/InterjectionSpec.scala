package com.jamesward.zio_decisions.orchestration

import com.jamesward.zio_decisions.TypeSafeMock
import com.jamesward.zio_decisions.TypeSafeMock.MockBehavior.Respond
import com.jamesward.zio_decisions.typesafe.Wire
import com.jamesward.zio_decisions.orchestration.Interjection.{Route, Running}
import zio.*
import zio.test.*

object InterjectionSpec extends ZIOSpecDefault:

  private def pick(id: String) = Respond(Map("route" -> Wire.Answer.Choice(id, Map(id -> 0.8, "new_task" -> 0.2), 0.8)), Wire.Usage(70, 3))

  private val running = List(
    Running(1, "fix the SAVE10 bug", 12000, List("search_text SAVE10", "read_file Discounts.scala")),
    Running(2, "add SPRING20", 3000),
  )

  def spec = suite("Interjection")(
    test("offers a new task, and steering or following each running task; Jev's pick is the route") {
      for
        mock     <- TypeSafeMock.tracked(pick("steer_1"), pick("after_2"), pick("new_task"), pick("stop_2"), pick("status"), pick("stop_process_7"))
        steer    <- Interjection.route("also trim whitespace", running).provideLayer(mock.decisions)
        after    <- Interjection.route("then commit it", running).provideLayer(mock.decisions)
        fresh    <- Interjection.route("when is shipping free?", running).provideLayer(mock.decisions)
        stop     <- Interjection.route("never mind the SPRING20 code", running).provideLayer(mock.decisions)
        status   <- Interjection.route("what's it doing?", running).provideLayer(mock.decisions)
        procs     = List(Interjection.Process(7, "call", "execute_terminal_command", Some(1), 600, 600, Some("./gradlew test")))
        kill     <- Interjection.route("kill that gradle run", running, None, procs).provideLayer(mock.decisions)
        requests <- mock.requests
        options   = requests.head.body.asObject.flatMap(_.get("questions")).flatMap(_.asObject).flatMap(_.get("route"))
                      .flatMap(_.asObject).flatMap(_.get("criteria")).flatMap(_.asObject).toList.flatMap(_.fields.map(_._1))
      yield assertTrue(
        steer.route == Route.Steer(1), after.route == Route.After(2), fresh.route == Route.NewTask, stop.route == Route.Stop(2),
        status.route == Route.Status, kill.route == Route.StopProcess(7),
        options.toSet == Set("new_task", "steer_1", "after_1", "stop_1", "steer_2", "after_2", "stop_2", "status"),
        requests.last.body.toString.contains("stop_process_7"), requests.last.body.toString.contains("./gradlew test"),
        requests.head.body.toString.contains("read_file Discounts.scala"),
        requests.head.body.toString.contains("also trim whitespace"),
        steer.usage.inputTokens == 70,
      )
    },
  )
