package com.jamesward.zio_decisions.orchestration

import zio.*
import zio.test.*

/** The work manager a plan's parts and a decision's batch share. */
object WorkSpec extends ZIOSpecDefault:

  def spec = suite("Work")(
    test("items run at the same time, at most parallelism at once, results in order; a failure is its item's alone") {
      for
        running <- Ref.make(0)
        most    <- Ref.make(0)
        item     = (i: Int) => Work.Item[Any, Int](s"item $i", Work.Kind.Call,
                     (running.updateAndGet(_ + 1).flatMap(n => most.update(_ max n)) *> ZIO.sleep(10.millis) *> running.update(_ - 1))
                       *> (if i == 3 then ZIO.fail(RuntimeException("three")) else ZIO.succeed(i)))
        done    <- Work.all((1 to 10).toList.map(item), parallelism = 4).fork.flatMap(f => TestClock.adjust(1.second) *> f.join)
        atOnce  <- most.get
        wide    <- Ref.make(0)
        _       <- most.set(0)
        _       <- Work.all((1 to 40).toList.map(item)).fork.flatMap(f => TestClock.adjust(1.second) *> f.join)
        dflt    <- most.get
      yield assertTrue(
        done.map(_.name) == (1 to 10).toList.map(i => s"item $i"),
        done(2).last.result.isLeft, done.filterNot(_.name == "item 3").forall(_.last.result.isRight),
        atOnce == 4, dflt == Work.DefaultParallelism,
      )
    },
    test("a round past its limit times out; another round when again says so; none after success or a stop") {
      for
        tries  <- Ref.make(0)
        slow    = Work.Item[Any, String]("slow", Work.Kind.Loop, tries.update(_ + 1) *> ZIO.never, timeout = Some(1.second),
                    again = Some(r => ZIO.succeed(Option.when(!r.stopped)(Work.Item[Any, String]("slow (again)", Work.Kind.Loop, ZIO.succeed("ok"))))))
        unfinished = Work.Item[Any, String]("unfinished", Work.Kind.Call, ZIO.succeed("unable"), succeeded = Some(_ == "done"),
                    again = Some(_ => ZIO.succeed(Some(Work.Item[Any, String]("unfinished (again)", Work.Kind.Call, ZIO.succeed("done"))))))
        loops  <- Ref.make(List.empty[Option[String]])
        named   = Work.Item[Any, String]("part a", Work.Kind.Loop, ParallelLoop.currentLoop.get.tap(l => loops.update(l :: _)).as("x"))
        done   <- Work.all(List(slow, unfinished, named)).fork.flatMap(f => TestClock.adjust(2.seconds) *> f.join)
        seen   <- loops.get
      yield assertTrue(
        done(0).rounds.map(_.name) == List("slow", "slow (again)"), done(0).rounds.head.result.left.exists(_.isInstanceOf[Work.TimedOut]),
        done(1).rounds.map(_.result.toOption) == List(Some("unable"), Some("done")),
        seen == List(Some("part a")),
      )
    },
  )
