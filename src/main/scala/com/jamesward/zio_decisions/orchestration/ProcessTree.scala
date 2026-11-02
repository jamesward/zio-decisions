package com.jamesward.zio_decisions.orchestration

import zio.*

import java.time.Instant

/**
 * The agent's work as a tree of processes: tasks, the loops working on them (a parallel run's gathering, parts and
 * integration), and their tool calls, each with its state, when it started, when it was last active, and a way to stop
 * it. The tree is carried on the fiber ([[ProcessTree.current]]): [[ProcessTree.run]] registers `zio` as a child of the
 * fiber's current node, runs it as that node, and records how it ended, so every loop and tool call below a task
 * registers itself without being handed the tree.
 *
 * Stopping a node ([[stop]]) interrupts it: a tool call that's stopped fails with [[Stopped]], a step like any failed
 * call, and the loop that made it goes on; a stopped loop or task ends, and what's above it sees that as its outcome.
 * Nothing waits on a node it can't stop, so a hung call never blocks what's above it for longer than someone takes to
 * notice it ([[stalled]]) and stop it.
 *
 * A task or loop node (an agent, a sub-agent: a parallel part) has a goal: what it works toward. A goal is set as the
 * node starts and can change while it runs ([[setGoal]], [[removeGoal]]: the user's messages steer it, the decision
 * model narrows or drops it); each change is kept with why, and published like any other change of the node.
 */
final class ProcessTree private (nodes: Ref[Map[ProcessTree.Id, ProcessTree.Node]], ids: Ref[Long], stops: Ref[Map[ProcessTree.Id, Promise[Nothing, String]]],
    starts: Hub[ProcessTree.Node], ends: Ref[Map[ProcessTree.Id, Promise[Nothing, ProcessTree.Node]]], changed: Hub[ProcessTree.Node]):
  import ProcessTree.*

  /** The nodes started from now on, as they start: who watches the tree is told, rather than polling it. */
  def started: ZIO[Scope, Nothing, Dequeue[Node]] = starts.subscribe

  /** Every node as it starts and as it ends (its state then): for who mirrors the tree elsewhere, without polling it. */
  def changes: ZIO[Scope, Nothing, Dequeue[Node]] = changed.subscribe

  /** The node as it ended, when it ends; at once when it already has (or isn't known). */
  def awaitEnd(id: Id): UIO[Option[Node]] =
    ends.get.map(_.get(id)).flatMap:
      case Some(p) => p.await.asSome
      case None    => nodes.get.map(_.get(id))

  /** Every node, oldest first. */
  def snapshot: UIO[Vector[Node]] = nodes.get.map(_.values.toVector.sortBy(_.id.value))

  /** The running nodes, oldest first. */
  def running: UIO[Vector[Node]] = snapshot.map(_.filter(_.state == State.Running))

  /** The running nodes of `kind` that have been running at least `after` without a sign of life. */
  def stalled(after: Duration, kind: Option[Kind] = Some(Kind.Call)): UIO[Vector[Node]] =
    Clock.instant.flatMap: now =>
      running.map(_.filter(n => kind.forall(_ == n.kind) && java.time.Duration.between(n.lastActive, now).toMillis >= after.toMillis))

  /** A node and its descendants. */
  def subtree(id: Id): UIO[Vector[Node]] =
    snapshot.map: all =>
      def below(i: Id): Vector[Node] = all.filter(_.parent.contains(i)).flatMap(n => n +: below(n.id))
      all.find(_.id == id).toVector ++ below(id)

  /**
   * Stops a running node and so everything below it, with `reason`; false when there's no such running node. A node
   * stopped ends [[State.Stopped]].
   */
  def stop(id: Id, reason: String): UIO[Boolean] =
    stops.get.map(_.get(id)).flatMap:
      case Some(p) => p.succeed(reason)
      case None    => ZIO.succeed(false)

  /** `id`'s goal is now `goal`, for `why` (the user's message, the decision model's judgment); false without such a running node. */
  def setGoal(id: Id, goal: String, why: String): UIO[Boolean] = changeGoal(id, Some(goal), why)

  /** `id` no longer has a goal (it was dropped, done another way), for `why`; false without such a running node. */
  def removeGoal(id: Id, why: String): UIO[Boolean] = changeGoal(id, None, why)

  /** The goal of `id`, or of its nearest ancestor that has one: what the node's work is toward. */
  def goalOf(id: Id): UIO[Option[String]] =
    nodes.get.map: all =>
      Iterator.iterate(all.get(id))(_.flatMap(_.parent).flatMap(all.get)).takeWhile(_.isDefined).flatten
        .collectFirst { case n if n.goal.isDefined => n.goal.get }

  private def changeGoal(id: Id, goal: Option[String], why: String): UIO[Boolean] =
    Clock.instant.flatMap: now =>
      nodes.modify: all =>
        all.get(id).filter(_.state == State.Running) match
          case None => (None, all)
          case Some(n) =>
            val changed = n.copy(goal = goal, goals = n.goals :+ GoalChange(now, goal, why), lastActive = now)
            (Some(changed), all.updated(id, changed))
      .flatMap(_.fold(ZIO.succeed(false))(n => changed.publish(n).as(true)))

  /** Records a sign of life of `id` (progress the node reports: a decision made, a line of output). */
  def touch(id: Id, detail: Option[String] = None): UIO[Unit] =
    Clock.instant.flatMap: now =>
      nodes.update(_.updatedWith(id)(_.map(n => n.copy(lastActive = now, detail = detail.orElse(n.detail)))))

  /** Runs `zio` as a new node under `parent`, recording its start, its end, and how it ended. */
  def child[R, E, A](parent: Option[Id], kind: Kind, name: String, detail: Option[String] = None, goal: Option[String] = None)(
      zio: Id => ZIO[R, E, A])(using stoppedAs: StoppedAs[E]): ZIO[R, E, A] =
    ZIO.uninterruptibleMask: restore =>
      for
        id    <- ids.updateAndGet(_ + 1).map(Id(_))
        now   <- Clock.instant
        stop  <- Promise.make[Nothing, String]
        ended <- Promise.make[Nothing, Node]
        node   = Node(id, parent, kind, name, detail, State.Running, now, now, None, None, goal,
                   goal.map(g => GoalChange(now, Some(g), "the work's start")).toVector)
        _     <- nodes.update(_.updated(id, node))
        _     <- stops.update(_.updated(id, stop))
        _     <- ends.update(_.updated(id, ended))
        _     <- starts.publish(node)
        _     <- changed.publish(node)
        // `exit` first: a race waits for the first *success*, so a failing node would wait for a stop forever
        raced <- restore(current.locally(Some(this -> id))(zio(id)).exit.raceEither(stop.await)).exit
        end   <- Clock.instant
        exit   = raced match
                   case Exit.Success(Left(inner))   => inner.mapExit(Left(_))
                   case Exit.Success(Right(reason)) => Exit.succeed(Right(reason))
                   case Exit.Failure(c)             => Exit.failCause(c)
        outcome = exit match
                    case Exit.Success(Left(_))       => (State.Done, None)
                    case Exit.Success(Right(reason)) => (State.Stopped, Some(reason))
                    case Exit.Failure(c) if c.isInterruptedOnly => (State.Stopped, Some("interrupted"))
                    case Exit.Failure(c)             =>
                      val why = c.failureOption.map {
                        case t: Throwable => Option(t.getMessage).getOrElse(t.toString)
                        case other        => String.valueOf(other)
                      }.getOrElse(c.prettyPrint.linesIterator.nextOption().getOrElse("failed"))
                      (State.Failed, Some(why))
        _     <- nodes.update(_.updatedWith(id)(_.map(n => n.copy(state = outcome._1, ended = Some(end), lastActive = end, outcome = outcome._2))))
        _     <- stops.update(_ - id)
        last  <- nodes.get.map(_.get(id))
        _     <- ZIO.foreachDiscard(last)(ended.succeed)
        _     <- ZIO.foreachDiscard(last)(changed.publish)
        _     <- ends.update(_ - id)
        _     <- prune
        out   <- exit match
                   case Exit.Success(Left(a))       => ZIO.succeed(a)
                   case Exit.Success(Right(reason)) => ZIO.fail(stoppedAs(Stopped(name, reason)))
                   case Exit.Failure(c)             => ZIO.failCause(c)
      yield out

  /** Keeps the tree's ended nodes to the last [[Keep]], so a long session's tree doesn't grow without end. */
  private def prune: UIO[Unit] =
    nodes.update: all =>
      val ended = all.values.filter(_.state != State.Running).toVector.sortBy(_.id.value)
      if ended.size <= Keep then all
      else
        val running = all.values.filter(_.state == State.Running).map(_.id).toSet
        // never an ended node that a running one is below
        def ancestors(i: Id): Set[Id] = all.get(i).flatMap(_.parent).fold(Set.empty[Id])(p => ancestors(p) + p)
        val needed = running.flatMap(ancestors)
        all -- ended.dropRight(Keep).map(_.id).filterNot(needed)

object ProcessTree:
  final case class Id(value: Long) derives CanEqual:
    override def toString: String = value.toString

  enum Kind derives CanEqual:
    /** A session: the tasks of one chat, below it; a tree of several sessions has one node per session. */
    case Session, Task, Loop, Call

  enum State derives CanEqual:
    case Running, Done, Failed, Stopped

  /** A process: what it is, where it is in the tree, its state, and its times. */
  final case class Node(
    id: Id,
    parent: Option[Id],
    kind: Kind,
    name: String,
    detail: Option[String],
    state: State,
    started: Instant,
    lastActive: Instant,
    ended: Option[Instant],
    outcome: Option[String],
    /** What the node works toward (a task's request, a part's task), as it is now. */
    goal: Option[String] = None,
    /** How its goal came to be: set, changed, removed, and why, oldest first. */
    goals: Vector[GoalChange] = Vector.empty,
  ):
    def runningMs(now: Instant): Long = java.time.Duration.between(started, ended.getOrElse(now)).toMillis
    def idleMs(now: Instant): Long = java.time.Duration.between(lastActive, ended.getOrElse(now)).toMillis

  /** A change of a node's goal: when, to what (none: removed), and why. */
  final case class GoalChange(at: Instant, goal: Option[String], why: String)

  /** Why a node ended early: someone stopped it. */
  final case class Stopped(name: String, reason: String) extends RuntimeException(s"$name was stopped: $reason")

  /** How a node's error type carries [[Stopped]]. */
  trait StoppedAs[E]:
    def apply(s: Stopped): E

  object StoppedAs:
    given throwable[E >: Stopped]: StoppedAs[E] = s => s

  /** How many ended nodes a tree keeps. */
  val Keep = 400

  def make: UIO[ProcessTree] =
    for
      nodes <- Ref.make(Map.empty[Id, Node])
      ids   <- Ref.make(0L)
      stops <- Ref.make(Map.empty[Id, Promise[Nothing, String]])
      hub   <- Hub.unbounded[Node]
      ends  <- Ref.make(Map.empty[Id, Promise[Nothing, Node]])
      chg   <- Hub.unbounded[Node]
    yield ProcessTree(nodes, ids, stops, hub, ends, chg)

  /** The fiber's tree and the node it works as, when it works within one. */
  val current: FiberRef[Option[(ProcessTree, Id)]] = Unsafe.unsafely(FiberRef.unsafe.make(Option.empty[(ProcessTree, Id)]))

  /**
   * Runs `zio` as a child of the fiber's current node, in the fiber's tree; as it is, outside any tree. Failures that
   * aren't throwables are kept as they are (a [[Stopped]] node fails with it only where `E` can carry it).
   */
  def run[R, E, A](kind: Kind, name: String, detail: Option[String] = None, goal: Option[String] = None)(zio: ZIO[R, E, A])(
      using StoppedAs[E]): ZIO[R, E, A] =
    current.get.flatMap:
      case Some((tree, parent)) => tree.child(Some(parent), kind, name, detail, goal)(_ => zio)
      case None                 => zio

  /** The fiber's current node's goal is now `goal`, for `why`. */
  def setGoal(goal: String, why: String): UIO[Unit] =
    current.get.flatMap(_.fold(ZIO.unit)((tree, id) => tree.setGoal(id, goal, why).unit))

  /** The fiber's current node has no goal any more, for `why`. */
  def removeGoal(why: String): UIO[Unit] =
    current.get.flatMap(_.fold(ZIO.unit)((tree, id) => tree.removeGoal(id, why).unit))

  /** The fiber's current node's goal (or its nearest ancestor's). */
  def goal: UIO[Option[String]] = current.get.flatMap(_.fold(ZIO.none)((tree, id) => tree.goalOf(id)))

  /** Records a sign of life of the fiber's current node. */
  def touch(detail: Option[String] = None): UIO[Unit] =
    current.get.flatMap(_.fold(ZIO.unit)((tree, id) => tree.touch(id, detail)))

  /**
   * The tree as text, one line per node, indented by depth: its id, kind, name, state, how long it has run (or ran)
   * and, for one running, how long since its last sign of life when that's long; and what it's doing or how it ended.
   */
  def render(nodes: Vector[Node], now: Instant, idleShownAfter: Duration = 30.seconds): String =
    def secs(ms: Long) = if ms < 10000 then f"${ms / 1000.0}%.1fs" else s"${ms / 1000}s"
    def line(n: Node, depth: Int): String =
      val state = n.state match
        case State.Running =>
          val idle = n.idleMs(now)
          s"running ${secs(n.runningMs(now))}" + (if idle >= idleShownAfter.toMillis then s", nothing for ${secs(idle)}" else "")
        case other => s"${other.toString.toLowerCase} after ${secs(n.runningMs(now))}"
      val what = n.detail.filter(_.nonEmpty).map(d => s" — ${d.take(160)}").getOrElse("")
      val why = n.outcome.filter(_ => n.state != State.Done).map(o => s" (${o.take(160)})").getOrElse("")
      // a goal that isn't the detail shown already, and one that changed, with its last change's reason
      val changed = n.goals.size > 1 || n.goals.lastOption.exists(_.goal.isEmpty)
      def one(g: String) = g.replace('\n', ' ').take(200)
      val goal = Option.when(changed)(n.goals.last).map(c => s"\n${"  " * (depth + 1)}goal ${c.goal.fold("removed")(g => s"now: ${one(g)}")}")
        .orElse(n.goal.filter(g => !n.detail.exists(d => g.startsWith(d.stripSuffix("…")))).map(g => s"\n${"  " * (depth + 1)}goal: ${one(g)}"))
        .getOrElse("") + (if changed then n.goals.lastOption.fold("")(c => s" (${c.why.take(120)})") else "")
      s"${"  " * depth}[${n.id}] ${n.kind.toString.toLowerCase} ${n.name}: $state$what$why$goal"
    val byParent = nodes.groupBy(_.parent)
    val ids = nodes.map(_.id).toSet
    def walk(n: Node, depth: Int): Vector[String] =
      line(n, depth) +: byParent.getOrElse(Some(n.id), Vector.empty).sortBy(_.id.value).flatMap(walk(_, depth + 1))
    nodes.filter(n => n.parent.forall(p => !ids(p))).sortBy(_.id.value).flatMap(walk(_, 0)).mkString("\n")
