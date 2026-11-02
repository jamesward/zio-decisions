package com.jamesward.zio_decisions.orchestration

import com.jamesward.zio_decisions.DecisionModel
import zio.*

import java.time.Instant

/**
 * Push coordination between loops working at the same time: what one does that bears on another's work (an edit of a
 * file it read, a check that failed after its edit) is told to it as it happens, rather than found at its next look.
 *
 * A host publishes [[Notice]]s on a [[Bus]] (from its own record of the work: an event log, say). Each loop that runs
 * with a bus in [[bus]] opens an [[Inbox]] on it, for its loop id ([[ParallelLoop.currentLoop]]): a notice reaches it
 * when one of its subjects is one of the loop's interests, which the host adds as the loop works ([[interest]]: the
 * files it reads and edits, the checks it runs), and when it's from another loop. What the inbox does with it is the
 * [[Policy]]'s:
 *
 *  - `Off`: nothing;
 *  - `Note`: every notice that reaches it is in the context of the loop's next decision;
 *  - `Judge`: the decision model also judges each, with what the loop is doing: one that makes its current work wrong
 *    or out of date is marked so, above the others, for the next decision to act on;
 *  - `Interrupt`: as `Judge`, and such a notice also stops the loop's work in flight (its arguments being written, a
 *    call that changes nothing being made: [[Inbox.interruptible]]), which ends as a failed step saying why, so the next
 *    decision is made with the notice in view. A call that changes things (an edit) is never stopped.
 */
object Notices:

  /** Something that happened: by which loop (`from`), of what kind, about what (`subjects`: files, say), in words. */
  final case class Notice(seq: Long, from: String, kind: String, subjects: Set[String], text: String, at: Instant)

  enum Policy(val id: String) derives CanEqual:
    case Off       extends Policy("off")
    case Note      extends Policy("note")
    case Judge     extends Policy("judge")
    case Interrupt extends Policy("interrupt")

  object Policy:
    def of(id: String): Option[Policy] = values.find(_.id == id.trim.toLowerCase)

  /**
   * What a failure caused by another loop's change starts with (work stopped by a notice; a host's edit refused because
   * its file changed since it was read): the same call made again isn't a repeat of a failed one, since what made it
   * fail was the world, not its arguments.
   */
  val OutOfDate = "out of date: "

  /** Work stopped by a notice that made it out of date. */
  final case class Superseded(notice: Notice)
      extends RuntimeException(s"${OutOfDate}stopped, another task's change made this out of date (${notice.text.take(600)})")

  /** The notices of the loops sharing it, in the order they were published. */
  final class Bus private (hub: Hub[Notice], seq: Ref[Long]):
    def publish(from: String, kind: String, subjects: Set[String], text: String): UIO[Unit] =
      (seq.updateAndGet(_ + 1) <*> Clock.instant).flatMap((n, at) => hub.publish(Notice(n, from, kind, subjects, text, at))).unit
    def subscribe: ZIO[Scope, Nothing, Dequeue[Notice]] = hub.subscribe

  object Bus:
    def make: UIO[Bus] = (Hub.unbounded[Notice] <*> Ref.make(0L)).map(Bus(_, _))

  /** The bus the loops of this fiber (and its children) share, when there is one. */
  val bus: FiberRef[Option[Bus]] = Unsafe.unsafely(FiberRef.unsafe.make(Option.empty[Bus]))

  /** The current loop's inbox, when it has one: what [[interest]] adds to. */
  val inbox: FiberRef[Option[Inbox]] = Unsafe.unsafely(FiberRef.unsafe.make(Option.empty[Inbox]))

  /** `subjects` added to the current loop's interests (a file it read, say); nothing outside a loop with an inbox. */
  def interest(subjects: Set[String]): UIO[Unit] = inbox.get.flatMap(ZIO.foreachDiscard(_)(_.interest(subjects)))

  /** A notice on the current fiber's bus, from the current loop; nothing without a bus. */
  def publish(kind: String, subjects: Set[String], text: String): UIO[Unit] =
    (bus.get <*> ParallelLoop.currentLoop.get).flatMap((b, loop) => ZIO.foreachDiscard(b)(_.publish(loop.getOrElse(""), kind, subjects, text)))

  /** A notice, as the inbox keeps it: `urgent` when the decision model judged it makes the loop's current work out of date. */
  final case class Received(notice: Notice, urgent: Boolean, p: Option[Double])

  /** A loop's notices: those for it, kept until its next decision takes them. */
  final class Inbox private[Notices] (
    val self: String,
    val policy: Policy,
    subjects: Ref[Set[String]],
    pending: Ref[Vector[Received]],
    signal: Ref[Promise[Nothing, Notice]],
    val doing: Ref[Map[Long, String]],
    ids: Ref[Long],
  ):
    def interest(more: Set[String]): UIO[Unit] = subjects.update(_ ++ more)
    def interests: UIO[Set[String]] = subjects.get

    /** Whether `n` is for this loop: another loop's, about one of its interests. */
    def wants(n: Notice): UIO[Boolean] =
      subjects.get.map(mine => n.from != self && n.subjects.exists(mine.contains))

    /** The notices since the last take, oldest first. */
    def take: UIO[Vector[Received]] = pending.getAndSet(Vector.empty)

    /** `work` (described as `what`) as in flight: judged against, and, under `Interrupt`, stopped by an urgent notice. */
    def inFlight[A](what: String, interruptible: Boolean)(work: Task[A]): Task[A] =
      ids.updateAndGet(_ + 1).flatMap: id =>
        val tracked = ZIO.acquireReleaseWith(doing.update(_.updated(id, what)))(_ => doing.update(_ - id))(_ => work)
        if policy != Policy.Interrupt || !interruptible then tracked
        else signal.get.flatMap(s => tracked.raceFirst(s.await.flatMap(n =>
          ZIO.logInfo(s"notices: $self stopped $what: ${n.kind} from ${n.from}") *> ZIO.fail(Superseded(n)))))

    private[Notices] def receive(r: Received): UIO[Unit] =
      pending.update(_ :+ r) *>
        (Promise.make[Nothing, Notice].flatMap(next => signal.getAndSet(next)).flatMap(_.succeed(r.notice)).unit)
          .when(r.urgent && policy == Policy.Interrupt).unit

  /**
   * The current loop's inbox on the fiber's bus, for the scope, under `policy` (none without a bus, or `Off`): the notices
   * for it gathered as they come, each judged by the decision model under `Judge` and `Interrupt`, with the loop's
   * `request` and what's in flight. Set as [[inbox]] for the scope.
   */
  def open(request: String, policy: Policy): ZIO[DecisionModel & Scope, Nothing, Option[Inbox]] =
    (bus.get <*> ParallelLoop.currentLoop.get).flatMap:
      case (Some(b), loop) if policy != Policy.Off =>
        for
          queue    <- b.subscribe
          box      <- (Ref.make(Set.empty[String]) <*> Ref.make(Vector.empty[Received]) <*> Promise.make[Nothing, Notice].flatMap(Ref.make(_)) <*>
                        Ref.make(Map.empty[Long, String]) <*> Ref.make(0L))
                        .map((i, p, s, d, n) => Inbox(loop.getOrElse(""), policy, i, p, s, d, n))
          decisions <- ZIO.environment[DecisionModel]
          _        <- queue.take.flatMap: n =>
                        box.wants(n).flatMap: wanted =>
                          if !wanted then ZIO.unit
                          else if policy == Policy.Note then
                            ZIO.logInfo(s"notices: ${box.self} heard ${n.kind} from ${n.from}") *> box.receive(Received(n, urgent = false, None))
                          else
                            (box.doing.get <*> box.interests).flatMap((now, mine) =>
                              judge(request, now.values.toList, n, mine).provideEnvironment(decisions)).either.flatMap:
                              case Right(p) =>
                                ZIO.logInfo(f"notices: ${box.self} judged ${n.kind} from ${n.from}: p $p%.2f${if p >= UrgentAt then " (urgent)" else ""}") *>
                                  box.receive(Received(n, urgent = p >= UrgentAt, Some(p)))
                              case Left(e)  => ZIO.logDebug(s"notices: couldn't judge ${n.kind}: ${e.getMessage}") *>
                                                 box.receive(Received(n, urgent = false, None))
                      .forever.forkScoped
          _        <- inbox.locallyScoped(Some(box))
        yield Some(box)
      case _ => ZIO.none

  /** A judged notice at or above this is urgent. */
  val UrgentAt = 0.6

  /** The decision model's probability that `n` makes what the loop is doing (`doing`, else its request) out of date. */
  def judge(request: String, doing: List[String], n: Notice, works: Set[String] = Set.empty): ZIO[DecisionModel, Throwable, Double] =
    val now = if doing.isEmpty then "between steps" else doing.mkString("; ")
    val on = if works.isEmpty then "" else s"\nWhat its work so far has read, edited or checked: ${works.toList.sorted.take(30).mkString(", ").take(1500)}"
    Judge.yesNo(
      s"A task works on this request:\n${request.take(1500)}\n\nWhat it's doing now: ${now.take(1500)}$on\n\n" +
        s"Another task working on the same project at the same time just did this (${n.kind}):\n${n.text.take(3000)}",
      "Does what the other task just did make what this task is doing now wrong or out of date, so it should stop and " +
        "decide again with it in view?",
      "it changes what this task's current work depends on: a file it's writing from an older reading of, code its " +
        "running check or test is about, a result it's about to rely on, or it does what this task is about to do",
      "it doesn't touch what this task is doing now, or only adds to it in a way that leaves its current step right",
    )

  /** The notices taken for a decision, as context: the urgent ones first, to act on. */
  def sections(received: Vector[Received]): Sections =
    val (urgent, rest) = received.partition(_.urgent)
    Sections(
      urgent.map(r => Sections.Section(s"Another task just changed what your work depends on (${r.notice.kind}): take it " +
        "into account before going on, rather than working from what you saw before", r.notice.text, UrgentPriority, 1500)) ++
      rest.map(r => Sections.Section(s"Meanwhile, another task (${r.notice.kind})", r.notice.text, NotePriority, 400)))

  /** Above the other sessions' work in the context (36) and the project's (40); the urgent ones near steering's. */
  val UrgentPriority = 90
  val NotePriority = 45
