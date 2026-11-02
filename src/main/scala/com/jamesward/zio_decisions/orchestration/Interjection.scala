package com.jamesward.zio_decisions.orchestration

import com.jamesward.zio_decisions.{DecisionModel, Decisions}
import com.jamesward.zio_decisions.Decisions.{ChoiceCriteria, Content, DynamicAnswer, Question, QuestionId}
import zio.*
import zio.schema.{Schema, derived}
import zio.schema.codec.json.schemaJson

/**
 * A user's message that arrives while the agent is already working: the decision model decides, with one choice, what it is.
 *
 *   - [[Route.NewTask]]: a request of its own, independent of the running work, to start now, at the same time;
 *   - [[Route.Steer]]: it adds to, corrects or redirects a running task (a constraint, a missed detail, "use X instead",
 *     "stop that"), so it goes to that task's loop, which reads it at its next decision ([[AgentLoop.run]]'s `live`);
 *   - [[Route.After]]: a follow-up that needs a running task's result ("then commit it", "and explain what you
 *     changed"), to start when that task is done, from its reply;
 *   - [[Route.Stop]]: the user wants a running task stopped, with nothing else to do instead ("stop", "never mind",
 *     "cancel that"); a correction that says what to do instead is steering;
 *   - [[Route.Status]]: a question about the work itself, what's running, how far it got, what's stuck;
 *   - [[Route.StopProcess]]: stop one process of the work, not a whole task: a tool call or a loop the user names or
 *     describes ("kill that gradle run", "the scenario 4 agent is stuck, stop it").
 *
 * The decision model sees the message and, of each running task, its request, how long it has run, what it has done so far and the
 * steering it already got.
 */
object Interjection:

  /** A running process below the tasks (a loop, a tool call), as the decision model sees it, to route a message about it. */
  final case class Process(id: Long, kind: String, name: String, task: Option[Int], runningSeconds: Long, idleSeconds: Long,
      detail: Option[String]) derives Schema

  /** A running task as the decision model sees it: `progress` is its calls so far, in short, oldest first. */
  final case class Running(id: Int, request: String, elapsedMs: Long, progress: List[String] = Nil, steering: List[String] = Nil)

  enum Route derives CanEqual:
    case NewTask
    case Steer(task: Int)
    case After(task: Int)
    case Stop(task: Int)
    case Status
    case StopProcess(process: Long)

  /** the decision model's route, with its probabilities by option (`new_task`, `steer_<id>`, `after_<id>`) and what it cost. */
  final case class Decision(route: Route, probabilities: Map[String, Double], confidence: Double, usage: Decisions.Usage, latencyMs: Long)

  private final case class State(message: String, situation: String, context: Option[String]) derives Schema
  private final case class About(kind: String, meaning: String, task: Option[RunningView], process: Option[Process] = None) derives Schema
  private final case class RunningView(id: Int, request: String, runningForSeconds: Long, doneSoFar: List[String], steeredWith: List[String])
      derives Schema

  private val Id = QuestionId("route")

  /** At most this many processes are offered to stop. */
  val MaxProcesses = 20

  /**
   * The decision model's route for `message`, given the tasks `running` (at least one) and the processes below them that could be
   * stopped on their own (at most [[MaxProcesses]] offered: the longest-idle first).
   */
  def route(message: String, running: List[Running], context: Option[String] = None, processes: List[Process] = Nil)
      : ZIO[DecisionModel, Throwable, Decision] =
    def view(r: Running) = RunningView(r.id, r.request, r.elapsedMs / 1000, r.progress.takeRight(12), r.steering)
    val fresh: List[(String, Content | Null)] = List("new_task" -> Content(About(
      "new_task",
      "A request of its own, which the running tasks' work doesn't answer and which doesn't change what they should " +
        "do: start it now, alongside them. A question about something else, another change elsewhere.",
      None,
    )))
    val perTask: List[(String, Content | Null)] = running.flatMap: r =>
      List(
        s"steer_${r.id}" -> Content(About(
          "steer",
          "About this running task's own work: it adds a requirement or detail to it, corrects it, narrows or redirects " +
            "it, or says to stop or do it differently. Phrased as a reaction to what it's doing, or meaningless on its " +
            "own (\"also handle X\", \"no, use Y\", \"don't touch Z\", \"stop\").",
          Some(view(r)),
        )),
        s"stop_${r.id}" -> Content(About(
          "stop",
          "The user wants this task stopped and nothing done in its place: \"stop\", \"never mind\", \"cancel that\", " +
            "\"forget it\". Not a correction that says what to do instead (that's steering).",
          Some(view(r)),
        )),
        s"after_${r.id}" -> Content(About(
          "after",
          "A further request that needs this task's result first: it acts on what the task will have done or found " +
            "(\"then commit it\", \"and add a test for that fix\", \"after that, explain the change\"), without changing " +
            "the task itself.",
          Some(view(r)),
        )),
      )
    val status: List[(String, Content | Null)] = List("status" -> Content(About(
      "status",
      "A question about the work in progress itself: what's running, how far it got, how long it has been, whether " +
        "something is stuck or hung, what it's waiting for. A short check-in while tasks run is one too (\"how's it " +
        "going?\", \"any progress?\", \"still working?\", \"status?\"): it asks about them, not for a chat. Answered " +
        "from the agent's process tree; nothing is changed.",
      None,
    )))
    val perProcess: List[(String, Content | Null)] = processes.sortBy(p => -p.idleSeconds).take(MaxProcesses).map: p =>
      s"stop_process_${p.id}" -> Content(About(
        "stop_process",
        "Stop just this process (one tool call or one agent loop), not the whole task: the user names it or describes " +
          "it (\"kill that gradle run\", \"the build is hung, stop it\", \"stop the scenario 4 agent\"). Its task goes on. Not " +
          "an instruction about the code the agent writes (how it should cancel or stop things is steering).",
        None,
        Some(p),
      ))
    val options: Map[String, Content | Null] = (fresh ++ perTask ++ status ++ perProcess).toMap
    val situation =
      s"The agent is working on ${running.size} task${if running.size == 1 then "" else "s"} when the user sends this message. " +
        "Decide what the message is."
    for
      criteria <- ZIO.fromEither(ChoiceCriteria.fromContent(options)).mapError(IllegalArgumentException(_))
      asked    <- AgentLoop.ask(Content(State(message, situation, context)), List(Id -> Question.Choice(Content(situation).json, criteria)))
      (answers, usage, latency) = asked
      answer   <- answers.get(Id) match
                    case Some(DynamicAnswer.Choice(a)) => ZIO.succeed(a)
                    case other                         => ZIO.fail(IllegalStateException(s"The decision model gave no route: $other"))
    yield
      val route = answer.choice match
        case s"steer_$id" => id.toIntOption.fold(Route.NewTask)(Route.Steer(_))
        case s"after_$id" => id.toIntOption.fold(Route.NewTask)(Route.After(_))
        case s"stop_process_$id" => id.toLongOption.fold(Route.NewTask)(Route.StopProcess(_))
        case "status"     => Route.Status
        case s"stop_$id"  => id.toIntOption.fold(Route.NewTask)(Route.Stop(_))
        case _            => Route.NewTask
      Decision(route, answer.probabilities.map((k, p) => k -> p.unwrap), answer.confidence.unwrap, usage, latency)
