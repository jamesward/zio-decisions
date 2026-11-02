package com.jamesward.zio_decisions

import com.jamesward.zio_decisions.typesafe.TypeSafe
import zio.*
import zio.http.Client
import zio.test.*
import zio.test.TestAspect.*

/**
 * The shared happy-path scenarios against TypeSafe AI's live Jev, through `TypeSafe.live` (`TYPESAFE_API_KEY`, and
 * `TYPESAFE_DEFAULT_MODEL` if set). Paid; runs only when `TYPESAFE_API_KEY` is set (the project's `.env` sets it for
 * tests, through sbt-dotenv).
 */
object TypeSafeIntegrationSpec extends ZIOSpecDefault:

  private def asTest(s: SharedSpec.Scenario): Spec[DecisionModel, Any] =
    test(s.name)(s.run)

  def spec = suite("TypeSafe (Jev), live")(
    SharedSpec.happyPathScenarios.map(asTest)*
  ).provideSomeShared[Scope](Client.default, TypeSafe.live)
    @@ ifEnv("TYPESAFE_API_KEY")(_.trim.nonEmpty)
    @@ withLiveClock
    @@ withLiveSystem
    @@ timeout(60.seconds)
    @@ sequential
