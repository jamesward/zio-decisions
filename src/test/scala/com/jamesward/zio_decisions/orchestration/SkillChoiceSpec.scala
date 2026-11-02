package com.jamesward.zio_decisions.orchestration

import com.jamesward.zio_decisions.TypeSafeMock
import com.jamesward.zio_decisions.TypeSafeMock.MockBehavior.Respond
import com.jamesward.zio_decisions.typesafe.Wire
import com.jamesward.zio_decisions.orchestration.SkillChoice.Offered
import zio.*
import zio.test.*

object SkillChoiceSpec extends ZIOSpecDefault:
  private val skills = List(
    Offered("zen-of-scala", "Scala 3 / ZIO idioms for writing and reviewing Scala code", named = true),
    Offered("database-connection-management", "Managing datasources in JetBrains IDEs", named = false),
    Offered("zen-of-projects", "Project and build conventions", named = true),
  )

  def spec = suite("SkillChoice")(
    test("one question per skill, by its name and description (named ones marked); those Jev judges needed are loaded") {
      for
        mock   <- TypeSafeMock.tracked(Respond(Map(
                    "skill_000" -> Wire.Answer.Noul(0.9), "skill_001" -> Wire.Answer.Noul(0.1), "skill_002" -> Wire.Answer.Noul(0.3))))
        chosen <- SkillChoice.choose("fix the failing spec in Foo.scala", skills).provideLayer(mock.decisions)
        asked  <- mock.requests
        body    = asked.head.body.toString
      yield assertTrue(
        chosen == List(0), asked.size == 1,
        body.contains("zen-of-scala"), body.contains("Managing datasources in JetBrains IDEs"),
        body.contains("The project's agent instructions name this skill"),
      )
    },
    test("no skills: Jev isn't asked; at most MaxLoaded are loaded, the likeliest") {
      val many = (0 until 6).toList.map(i => Offered(s"s$i", s"skill $i", named = false))
      for
        none   <- TypeSafeMock.tracked(Respond(Map.empty))
        nothing <- SkillChoice.choose("hi", Nil).provideLayer(none.decisions)
        asked  <- none.requests
        all    <- TypeSafeMock.tracked(Respond((0 until 6).map(i => f"skill_$i%03d" -> Wire.Answer.Noul(0.5 + i * 0.05)).toMap))
        picked <- SkillChoice.choose("go", many).provideLayer(all.decisions)
      yield assertTrue(nothing.isEmpty, asked.isEmpty, picked == List(3, 4, 5))
    },
  )
