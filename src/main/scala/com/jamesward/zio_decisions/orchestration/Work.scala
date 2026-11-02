package com.jamesward.zio_decisions.orchestration

import zio.*

/**
 * Independent pieces of work run at the same time, however they came about: a plan's parts, each an agent loop
 * ([[ParallelLoop]]), or a decision's batch of calls, any tools ([[AgentLoop.Strategy.batch]],
 * [[AgentLoop.Strategy.parallelCalls]]). The two are the same thing triggered differently, so they share this. Each
 * item:
 *
 *  - runs as what it is: a [[Work.Kind.Loop]] is a node of the process tree, the loop its fiber works for
 *    ([[ParallelLoop.currentLoop]]: the host's tools coordinate by it) and its log lines' `loop`; a
 *    [[Work.Kind.Call]] runs as it is (a tool call registers its own node);
 *  - fails on its own: its failure is its outcome, never the batch's, and the others go on;
 *  - may have a time limit, past which its round fails with [[Work.TimedOut]];
 *  - may get more rounds: after one that didn't succeed, its `again` decides (a judge, a policy) whether there's
 *    another and what it is; one someone stopped gets none;
 *  - reports its progress (in progress, then completed).
 *
 * At most `parallelism` items run at once (default [[Work.DefaultParallelism]]); the rest wait their turn. Results
 * come in the items' order. Interrupting the batch interrupts every item.
 */
object Work:

  /** How many items run at once unless said otherwise. */
  val DefaultParallelism = 32

  enum Kind derives CanEqual:
    case Loop, Call

  /**
   * A piece of work: its name, what it is, the effect, its time limit, what counts as done (by default, not failing),
   * and what happens after a round that isn't (by default, nothing).
   */
  final case class Item[R, A](
    name: String,
    kind: Kind,
    run: ZIO[R, Throwable, A],
    timeout: Option[Duration] = None,
    succeeded: Option[A => Boolean] = None,
    again: Option[Round[A] => URIO[R, Option[Item[R, A]]]] = None,
    /** What the item's loop works toward: its node's goal in the process tree. */
    goal: Option[String] = None,
  )

  /** One round of an item: its name, how it ended, how long it took. */
  final case class Round[+A](name: String, result: Either[Throwable, A], ms: Long):
    def stopped: Boolean = result.left.exists(_.isInstanceOf[ProcessTree.Stopped])

  /** An item's rounds, first to last (never empty). */
  final case class Done[+A](name: String, rounds: List[Round[A]]):
    def last: Round[A] = rounds.last
    def ms: Long = rounds.map(_.ms).sum

  /** A round past its time limit: its item may get another (unlike one someone stopped). */
  final case class TimedOut(name: String, limit: Duration)
      extends RuntimeException(s"$name took longer than ${if limit.toMinutes > 0 then s"${limit.toMinutes} minutes" else s"${limit.toSeconds} seconds"}")

  /** Every item, at most `parallelism` at once, each to its last round; `progress` told as each starts and ends. */
  def all[R, A](
    items: List[Item[R, A]],
    parallelism: Int = DefaultParallelism,
    progress: (String, ParallelLoop.Progress) => UIO[Unit] = (_: String, _: ParallelLoop.Progress) => ZIO.unit,
  ): URIO[R, List[Done[A]]] =
    ZIO.foreachPar(items): item =>
      (progress(item.name, ParallelLoop.Progress.InProgress) *> rounds(item).map(Done(item.name, _)))
        <* progress(item.name, ParallelLoop.Progress.Completed)
    .withParallelism(parallelism.max(1))

  private def rounds[R, A](item: Item[R, A]): URIO[R, List[Round[A]]] =
    once(item).flatMap: round =>
      if round.result.exists(a => item.succeeded.forall(_(a))) || round.stopped then ZIO.succeed(List(round))
      else item.again.fold(ZIO.none)(_(round)).flatMap:
        case Some(next) => rounds(next).map(round :: _)
        case None       => ZIO.succeed(List(round))

  private def once[R, A](item: Item[R, A]): URIO[R, Round[A]] =
    val limited = item.timeout.fold(item.run)(t => item.run.timeoutFail(TimedOut(item.name, t))(t))
    val placed = item.kind match
      case Kind.Loop => loop(item.name, item.goal)(limited)
      case Kind.Call => limited
    placed.either.timed.map((elapsed, r) => Round(item.name, r, elapsed.toMillis))

  /**
   * Runs `zio` as loop `name`, within the loop the fiber already works for (`task-2/scenario 1`): a node of the process
   * tree (when there's one: it can be seen and stopped), its log lines annotated `loop=<that>`.
   */
  def loop[R, A](name: String, goal: Option[String] = None)(zio: ZIO[R, Throwable, A]): ZIO[R, Throwable, A] =
    ParallelLoop.currentLoop.get.flatMap: parent =>
      val full = parent.fold(name)(p => s"$p/$name")
      ProcessTree.run(ProcessTree.Kind.Loop, name, goal = goal)(ParallelLoop.currentLoop.locally(Some(full))(ZIO.logAnnotate("loop", full)(zio)))
