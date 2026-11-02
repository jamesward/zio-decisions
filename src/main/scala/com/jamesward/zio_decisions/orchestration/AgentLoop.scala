package com.jamesward.zio_decisions.orchestration

import com.jamesward.zio_decisions.{DecisionModel, Decisions}
import com.jamesward.zio_decisions.Decisions.{ChoiceAnswer, ChoiceCriteria, Content, DynamicAnswer,
  LoopTurn, NoulCriteria, Probability, Question, QuestionId, ScoreCriteria}
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.{Schema, derived}
import zio.schema.codec.json.schemaJson

/**
 * An agentic loop with the decision model as the orchestrator. At every step the decision model answers one choice: call one of the tools, each
 * offered with everything known about it (name, description, input and output JSON Schemas, annotations, how its earlier
 * calls went); read one of the documents (skills, references: guidance on how to use the tools or do the task), offered
 * with its title and description; or end the loop, either with an answer or by reporting that the request can't be
 * completed. The decision model only chooses; the host does the work of each choice:
 *
 *   - calling a tool: a generative model writes the tool's arguments (structured output against its input schema, from
 *     the request, the steps so far and the documents read), and the host calls the tool and records the result or the
 *     error as a step;
 *   - reading a document: the host reads it and keeps its text in the loop's state; the documents it links to become
 *     choices in turn, so knowledge loads progressively (a skill, then the reference it points to for one tool);
 *   - ending: the generative model writes the reply from the request, every step and the documents read, as an answer
 *     or as an explanation of what couldn't be done.
 *
 * The decision model sees the request, every step and every document read (each cut to [[AgentLoop.Config.resultPreview]]
 * characters). Once the tool-call budget is spent only the endings are offered, so the loop always ends with a reply.
 * The generative model is never given tools.
 */
object AgentLoop:

  /**
   * Where the loop says what it's doing between its tool calls (writing a call's arguments, writing the reply, going on
   * because the work isn't done, a course correction), for a host that shows it (an ACP client's "thoughts"); none by
   * default. Inherited by forked loops (a parallel run's parts).
   */
  val notes: FiberRef[Option[String => UIO[Unit]]] = Unsafe.unsafely(FiberRef.unsafe.make(Option.empty[String => UIO[Unit]]))

  /** Says `text` where [[notes]] goes. */
  def note(text: String): UIO[Unit] = notes.get.flatMap(_.fold(ZIO.unit)(_(text)))

  /**
   * One tool the decision model can choose, as its server describes it. With `schemasInChoice` false, the decision model's choice offers the tool by
   * its name, description, annotations and parameters in short (name, type, whether required), for toolsets too large
   * to offer with their schemas; the generative model still writes the arguments against `inputSchema`.
   */
  final case class Tool(
    name: String,
    description: String,
    inputSchema: Json.Obj,
    outputSchema: Option[Json.Obj] = None,
    annotations: Option[Json.Obj] = None,
    schemasInChoice: Boolean = true,
    group: Option[ToolGroup] = None,
  )

  /**
   * A group a tool is offered in when the catalog is large ([[Config.groupAbove]]): until the decision model opens the group (one
   * option, its name, description and its tools' names), its tools aren't offered one by one. A tool with no group is
   * always offered.
   */
  final case class ToolGroup(name: String, description: String) derives CanEqual

  /**
   * A document the decision model can choose to read: a skill or a reference. `id` is unique and stable across reads (documents linked
   * from several places are offered once); `metadata` is shown to the decision model with the title and description (a skill's
   * front matter, say).
   */
  final case class Document(
    id: String,
    title: String,
    description: String,
    read: Task[DocumentContent],
    metadata: Map[String, String] = Map.empty,
  )

  /** A document's text and the documents it links to, which become choices once it is read. */
  final case class DocumentContent(text: String, linked: Seq[Document] = Nil)

  /** A document that was read: its text, or why it couldn't be read. */
  final case class Reading(number: Int, document: String, title: String, text: Either[String, String])

  /** How a tool call ended: its result, or its error. */
  enum Outcome derives CanEqual:
    case Succeeded(result: Json)
    case Failed(error: String)

  /** One tool call of the loop. */
  final case class Step(number: Int, tool: String, arguments: Json.Obj, outcome: Outcome)

  /** How the loop ended. */
  enum Ending derives CanEqual:
    /** the decision model chose to answer from what the steps found (or directly, when no tool was needed). */
    case Answered
    /** the decision model chose to report that the request can't be completed with these tools. */
    case Unable
    /** The step budget ran out and the decision model chose to answer anyway; the reply says what's missing, if anything. */
    case BudgetExhausted

  /**
   * What the generative model needs to write a tool's arguments. `rejected` holds arguments it already wrote this time
   * that repeat an earlier failed call to the tool, so it writes something else.
   */
  final case class ArgumentsRequest(
    prompt: String,
    tool: Tool,
    steps: Vector[Step],
    readings: Vector[Reading] = Vector.empty,
    rejected: Vector[Json.Obj] = Vector.empty,
    context: Option[String] = None,
    offered: Seq[String] = Nil,
    problem: Option[String] = None,
  ):
    /** The earlier calls to this tool that failed. */
    def failures: Vector[Step] = steps.filter(s => s.tool == tool.name && s.outcome.isInstanceOf[Outcome.Failed])

  /**
   * The generative model's answer, asked for a tool's arguments, that another of the offered tools (`offered` in the
   * request) is the call to make, with its arguments: the step isn't made, and the next decision offers the decision model that call
   * ready-made (an option of its own, needing no model call), so the decision model decides, knowing what the model wanted.
   */
  final case class Proposed(tool: String, arguments: Json.Obj)
      extends RuntimeException(s"the arguments' writer would call $tool instead")

  /**
   * What the generative model needs to write the reply: besides the steps and readings, the tools and documents the
   * loop had, so a reply about the agent's capabilities describes these and nothing else.
   */
  final case class ReplyRequest(
    prompt: String,
    steps: Vector[Step],
    ending: Ending,
    readings: Vector[Reading] = Vector.empty,
    tools: Seq[Tool] = Nil,
    documents: Seq[Document] = Nil,
    context: Option[String] = None,
    problem: Option[String] = None,
  )

  /** The generative model the loop uses; it is never given tools. */
  trait Llm:
    def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]]
    def reply(request: ReplyRequest): Task[LlmResult[String]]

    /**
     * A workflow ([[Strategy.workflows]]): several calls written at once, to run in order. A model that can't write
     * one fails, and the step is a failed one like an argument that couldn't be written.
     */
    def workflow(request: WorkflowRequest): Task[LlmResult[List[PlannedCall]]] =
      ZIO.fail(UnsupportedOperationException("this generative model doesn't write workflows"))

  /** One call of a workflow: a tool and its arguments. */
  final case class PlannedCall(tool: String, arguments: Json.Obj)

  /**
   * What the generative model needs to write a workflow: the request, the tools it can use, what's been done. With
   * `concurrent`, the calls are a batch ([[Strategy.batch]]) that run at the same time, so none may depend on another.
   */
  final case class WorkflowRequest(
    prompt: String,
    tools: Seq[Tool],
    steps: Vector[Step],
    readings: Vector[Reading],
    context: Option[String],
    maxCalls: Int,
    concurrent: Boolean = false,
  )

  /** Calls a tool, failing with its error message when the tool fails. */
  trait Tools:
    def call(name: String, arguments: Json.Obj): Task[Json]

  /**
   * @param maxSteps      at most this many tool calls; the choice after the last one offers only the endings
   * @param maxReadings   at most this many documents read; after the last, no more are offered
   * @param resultPreview how much of each tool result (rendered as JSON) and document the decision model sees; the generative model
   *                      gets them in full
   * @param stateBudget   about how many characters of step results and readings the decision model sees in all: the newest get up to
   *                      `resultPreview` each, and once the budget is spent older ones are cut to [[OlderPreview]]
   * @param requestBudget about how many characters one decision request may have in all (The decision model rejects one past its limit,
   *                      about 114000): the options are what they are, and the steps (at most `stateBudget`) and the
   *                      context ([[Sections]], fitted by their priorities) share what's left. A request the decision model still
   *                      rejects as too long is made again with three quarters of the budget, twice at most.
   * @param llmContextBudget how many characters of the context the generative model gets (fitted the same way)
   * @param isCheck       which tools check the work (a test run, a build, a command): [[Strategy.correct]] has the decision model assess
   *                      those steps (their output says whether the work works); another tool's step worked when its
   *                      call succeeded
   * @param escalation    a stronger generative model, for [[Strategy.correct]]'s `escalate` move: once the decision model chooses it,
   *                      the loop's arguments and reply are written by it
   * @param instructions  the choice's instructions
   * @param checkDone     before the loop ends, the decision model reads its reply: one that says the request isn't done (not found, not
   *                      changed, a plan of what's left) sends the loop on, told what's missing, with another budget of
   *                      steps when it's out of them, at most [[MaxUnfinished]] times
   * @param exploreLimit  after this many steps in a row that only read (`explores`), each decision also asks the decision model whether
   *                      what's been read is enough to start the change the request asks for; once it says so, the next
   *                      decisions offer no reading tools (with guidance to make the change) until a step does more than
   *                      read; at [[ExploreCap]] times the limit, whatever the decision model judges. 0 turns it off. A loop that
   *                      reads and reads without acting is the commonest way a change request ends with nothing done.
   * @param reviewEvery   after this many calls since the last review, the decision also asks the decision model whether those calls
   *                      moved the request forward (found something it needs, changed something it asks for, or showed a
   *                      check's result changing); judged not (below [[HaltBelow]]), the loop is offered only the endings
   *                      and isn't sent on as unfinished: a loop that runs a hundred calls learning nothing ends, saying
   *                      what it found and what's in the way. 0 turns it off. The watchdog judges single stalled calls;
   *                      this judges the loop.
   * @param briefArguments before a call's arguments are written, the decision model picks the earlier steps, documents and project
   *                      context the writer needs (the rest are left out of its prompt), and the writer may ask for
   *                      more instead of guessing ([[Briefed]])
   * @param groupAbove    with more tools offered than this, those in a [[ToolGroup]] are offered as their group (an
   *                      option naming its tools) until the decision model opens it, which costs no step; a tool once called stays
   *                      offered. The tools with no group (the ones a request most often needs) are always offered.
   * @param explores      which tools only explore (read, search, fetch): by default, those with the `readOnlyHint`
   * @param endsTurn      tools whose successful call is the request's whole work (it starts something that goes on by
   *                      itself: a watch): after one, only the endings are offered
   * @param changes       which tools change the work (an edit): with `checkDone`, a loop that would end after one with no
   *                      check since goes on to check it first
   * @param maxParallel   how many calls a batch ([[Strategy.batch]]) may have, and how many of a decision's calls run at
   *                      once ([[Work]])
   * @param checkReply    the decision model checks the written reply: one that reads as the agent's own planning ("We need to call
   *                      X", the next steps it means to take) rather than a report to the user is written once more,
   *                      told so
   */
  final case class Config(
    maxSteps: Int = 12,
    maxReadings: Int = 8,
    resultPreview: Int = 4000,
    stateBudget: Int = 24000,
    requestBudget: Int = 75000,
    llmContextBudget: Int = 48000,
    instructions: String = DefaultInstructions,
    strategy: Strategy = Strategy(),
    escalation: Option[Llm] = None,
    isCheck: Tool => Boolean = Config.namedAsCheck,
    checkReply: Boolean = false,
    endsTurn: Tool => Boolean = _ => false,
    checkDone: Boolean = false,
    exploreLimit: Int = 0,
    reviewEvery: Int = 0,
    groupAbove: Int = 24,
    briefArguments: Boolean = false,
    explores: Tool => Boolean = Config.readOnlyHint,
    maxParallel: Int = Work.DefaultParallelism,
    changes: Tool => Boolean = t => !Config.readOnlyHint(t),
    notices: Notices.Policy = Notices.Policy.Off,
  )

  object Config:
    /** A tool annotated `readOnlyHint: true`. */
    val readOnlyHint: Tool => Boolean = t => t.annotations.exists(_.get("readOnlyHint").contains(Json.Bool(true)))

    /** A tool that checks the work (runs tests, builds, runs a command), by its name. */
    val namedAsCheck: Tool => Boolean = t =>
      List("test", "build", "compile", "command", "terminal", "run_", "problems", "lint", "check").exists(t.name.toLowerCase.contains)

  /**
   * Ways of running the loop that trade the decision model questions for tokens and generative-model calls; each is off by default.
   *
   * @param catalogInState  the tools' descriptions and parameters go in the state, once, and each tool's option only
   *                        names it, rather than every option carrying its tool's description
   * @param relevanceFilter before the first step, one decision request asks of every tool (a yes-or-no question each)
   *                        whether the request could need it, and only the likely ones are offered (at least
   *                        [[Strategy.MinRelevant]]), with an option to offer the rest
   * @param boundCalls      calls whose arguments are values earlier results hold are offered ready-made (a search hit's
   *                        `filePath` for a tool taking a file path, say): choosing one calls the tool with no
   *                        generative-model call
   * @param noRepeats       a call repeating one that already succeeded (same tool, same arguments) isn't made again
   *                        when only read-only calls (`readOnlyHint` annotation) succeeded since: the step says to use
   *                        the earlier result; after any other call (an edit, say) it runs again
   * @param parallelCalls   each step also asks the decision model for a second call to run at the same time, independent of the first
   *                        (another search, another file to read); both are written and called concurrently
   * @param correct         course correction: each decision's request also asks the decision model about the newest step (did it do
   *                        what it was meant to? does its failure repeat an earlier step's, with code changed between?
   *                        does it say what's wrong, or only that something failed?), shown with the step from then on;
   *                        after a step that didn't work, the next decision also offers moves that change how the work
   *                        goes rather than another call: diagnose (the next change shows why), reread the spec, try
   *                        another approach, escalate to a stronger model ([[Config.escalation]]). A move takes a
   *                        decision and no step, and its guidance stays with the loop (The decision model's state, the generative
   *                        model's prompts) until a step works
   * @param background      a check (a test run, a build) can also be started in the background: the loop goes on (reading,
   *                        editing) while it runs, and its result is a step at the first decision after it ends; with
   *                        nothing else to do, the decision model can choose to wait, which takes the next decision when one ends (it's
   *                        told, not polled: no sleeping and checking). Background calls still running at the end are
   *                        stopped
   * @param workflows       the decision model is also offered a workflow: the generative model writes several calls at once (edit,
   *                        build, run the tests), which run in order, each a step, stopping at the first that fails:
   *                        one writing and one decision for what would be several of each
   * @param giveUp          with `correct`, from the [[Strategy.GiveUpAfter]]th failed check on, the assessment also asks
   *                        whether the work is getting closer across the failed checks; when the decision model judges it isn't, only the
   *                        endings are offered: a loop going round in circles stops, saying what it tried
   * @param batch           the decision model is also offered a batch: the generative model writes up to [[Config.maxParallel]] calls
   *                        of any of the offered tools (files to read, pages to fetch, searches, edits of different
   *                        files, commands) that don't depend on each other's results, and they run at the same time
   *                        ([[Work]], as a plan's parts do), each a step. One decision and one writing for what would be
   *                        several of each, done one after another; the batch counts as one step of the budget
   */
  final case class Strategy(
    catalogInState: Boolean = false,
    relevanceFilter: Boolean = false,
    boundCalls: Boolean = false,
    parallelCalls: Boolean = false,
    noRepeats: Boolean = false,
    context: ContextChoice = ContextChoice.All,
    correct: Boolean = false,
    giveUp: Boolean = false,
    workflows: Boolean = false,
    background: Boolean = false,
    batch: Boolean = false,
  ):
    def describe: String =
      List("catalogInState" -> catalogInState, "relevanceFilter" -> relevanceFilter, "boundCalls" -> boundCalls,
        "parallelCalls" -> parallelCalls, "noRepeats" -> noRepeats,
        "keepContext" -> (context == ContextChoice.Keep), "rateContext" -> (context == ContextChoice.Rate),
        "correct" -> correct, "giveUp" -> giveUp, "workflows" -> workflows, "background" -> background,
        "batch" -> batch).collect { case (name, true) => name } match
        case Nil   => "default"
        case names => names.mkString("+")

  /**
   * Which earlier steps' results are in the context the decision model and the generative model see. The transcript itself (every
   * step, every result) is always kept whole, as the record; the context is a projection of it. With `All` every
   * result is in it (cut by the state budget); with `Keep` each decision also asks the decision model, of each earlier step whose
   * result is long, a yes-or-no question (is its result still needed?), and with `Rate` a 0–3 rating, kept from
   * [[Strategy.KeepRating]]. A result left out stays out of the decision model's next states and the generative model's prompts (the
   * step, its tool and arguments, stays in); the newest step is never judged, since nothing has used it yet.
   */
  enum ContextChoice derives CanEqual:
    case All, Keep, Rate

  object Strategy:
    /** With [[Strategy.workflows]], at most this many calls in one workflow. */
    val MaxWorkflowCalls = 5


    /** With [[Strategy.giveUp]], how many failed checks before the decision model is asked whether the work is getting closer. */
    val GiveUpAfter = 3

    /** Results shorter than this are always kept: leaving them out saves little. */
    val KeepShorterThan = 400

    /** With `Rate`, a step is kept when its expected rating (0–3) is at least this. */
    val KeepRating = 1.0

    /** At least this many tools are kept by the relevance filter, the likeliest first. */
    val MinRelevant = 6

    /** A tool is kept when the decision model's probability that the request could need it is at least this. */
    val RelevanceThreshold = 0.25

    /** At most this many ready-made calls are offered at a step, the newest results' first. */
    val MaxBound = 12

    /** The relevance filter runs only for more tools than this. */
    val FilterAbove = 12

  val DefaultInstructions: String =
    "You orchestrate an agent answering the user's request. Choose the next action: call the one tool that best " +
      "advances the request, given what the earlier steps found and how they failed; read a document when it explains " +
      "how to use a tool or do the task and you don't know yet (a skill first, then the reference it points to for the " +
      "tool you need); or end the loop. End with an answer " +
      "when the steps so far are enough, or when the request needs no tool at all (a greeting, general knowledge). End " +
      "as unable when no available tool can get what the request needs, or when the needed calls keep failing. Don't " +
      "repeat a call that already failed the same way. Option ids are opaque; judge their structured descriptions."

  /** The loop's result: the reply, how it ended, every step, and what each model used. */
  final case class Result(
    reply: String,
    ending: Ending,
    steps: Vector[Step],
    readings: Vector[Reading],
    decisions: List[Decisions.LoopTurn],
    decisionUsage: Decisions.Usage,
    llmCalls: Int,
    llmInputTokens: Long,
    llmOutputTokens: Long,
    llmLatencyMs: Long = 0,
    toolLatencyMs: Long = 0,
    leftOut: Set[Int] = Set.empty,
  ):
    /** The time the decision model's decisions took, together. */
    def decisionLatencyMs: Long = decisions.map(_.latencyMs).sum

  final case class TooManyTools(count: Int)
      extends RuntimeException(s"$count tools are more than a decision choice can offer (at most ${MaxTools})")

  /** A decision choice has at most 255 options, two of which are the endings. */
  val MaxTools: Int = Decisions.ChoiceCriteria.MaxOptions - 2

  private enum Action:
    case Call(tool: Tool)
    /** A call with arguments taken from earlier results: no generative model writes them. */
    case Bound(tool: Tool, arguments: Json.Obj, source: Int)
    case Read(document: Document)
    case End(ending: Ending)
    /** Offer the tools the relevance filter left out. */
    case MoreTools
    /** Offer a [[ToolGroup]]'s tools one by one. */
    case OpenGroup(group: ToolGroup)
    /** A course correction ([[Strategy.correct]]): no call; its guidance stays until a step works. */
    case Move(move: Correction)
    /** A workflow ([[Strategy.workflows]]): calls written at once, run in order. */
    case Workflow
    /** A batch ([[Strategy.batch]]): independent calls written at once, run at the same time. */
    case Batch
    /** A call started in the background ([[Strategy.background]]). */
    case Background(tool: Tool)
    /** Wait for the first background call to end ([[Strategy.background]]). */
    case Await
    /** Revise the goal the loop works toward (its node's in the process tree): narrow it, or drop it. */
    case ReviseGoal

  /**
   * The moves of [[Strategy.correct]]: what each tells the loop (The decision model's state, the generative model's prompts) until a
   * step works.
   */
  enum Correction(val id: String, val description: String, val guidance: String) derives CanEqual:
    case Diagnose extends Correction("diagnose",
      "The failures don't say what's wrong: make the next change one that shows why (log or print the values involved: " +
        "responses, statuses, ids, timings, what each step returned), run once, and read that before changing the logic.",
      "Course correction: diagnose. The failures so far don't say what's wrong. Make the next change add output that " +
        "shows why (the values involved: what each request returned, statuses, ids, timings), where the next run will " +
        "show it: a test runner may hide standard output, so print to standard error, put the values in the failure's " +
        "message, or run the code or the request directly (a command, a small program) rather than through the tests. " +
        "Run it once, and read that output before changing the logic again.")
    case Reread extends Correction("reread",
      "The code may not do what was asked: go back to the specification or the request's exact words and compare them " +
        "with the code, point by point, before the next change.",
      "Course correction: reread the specification. Before the next change, compare the code with the exact words of " +
        "the request and its specification (in the context), point by point; fix what differs, not what's guessed.")
    case Rethink extends Correction("rethink",
      "The same approach failed again: replace it with a different one (another algorithm, API or structure), not " +
        "another variation of the same code.",
      "Course correction: a different approach. The same approach has failed more than once; replace it with a " +
        "different one (another algorithm, API or structure), not a variation of the code that failed.")
    case Escalate extends Correction("escalate",
      "Hand the writing of this work's code and arguments to a stronger model, for the rest of the loop.",
      "")

  /** A call running in the background ([[Strategy.background]]): what, since when, and its fiber. */
  private final case class Running(tool: Tool, arguments: Json.Obj, afterStep: Int,
      fiber: Fiber[Nothing, (Option[Step], Usage, Long, Option[(Tool, Json.Obj)])])

  /** the decision model's assessment of a step ([[Strategy.correct]]), as the steps show it. */
  final case class Assessment(worked: Double, repeats: Option[Int], informative: Double, progressing: Option[Double] = None):
    def stuck: Boolean = failed && progressing.exists(_ < 0.3)
    def failed: Boolean = worked < 0.5
    def describe: String =
      if !failed then f"worked (p $worked%.2f)"
      else List(Some(f"didn't work (p ${1 - worked}%.2f)"), repeats.map(n => s"repeats step $n's failure"),
        Option.when(informative < 0.5)("says only that it failed, not why")).flatten.mkString("; ")

  /** The steps and readings so far, and the documents that can be read (in the order they became available). */
  private final case class State(
    steps: Vector[Step],
    readings: Vector[Reading],
    documents: Vector[Document],
    llm: Usage,
    toolLatencyMs: Long = 0,
    offered: Vector[Tool] = Vector.empty,
    filtered: Boolean = false,
    lastChoice: Map[String, Double] = Map.empty,
    dropped: Set[Int] = Set.empty,
    proposed: Vector[(Tool, Json.Obj, Int)] = Vector.empty,
    assessed: Map[Int, Assessment] = Map.empty,
    correction: Option[Correction] = None,
    movedAt: Option[Int] = None,
    escalated: Boolean = false,
    running: Vector[Running] = Vector.empty,
    unfinished: Vector[String] = Vector.empty,
    extraSteps: Int = 0,
    ready: Option[Double] = None,
    asksChange: Option[Double] = None,
    batched: Int = 0,
    reviewedAt: Int = 0,
    halted: Boolean = false,
    opened: Set[String] = Set.empty,
    /** The goal of the loop's node in the process tree (or its nearest ancestor's), read at each decision. */
    goal: Option[String] = None,
    /** How many times this loop revised its goal. */
    revisions: Int = 0,
  ):
    /**
     * The steps as the generative model sees them: the full transcript (`steps`, every result, always kept: what
     * repeats and bindings are checked against, and the loop's result) projected through the decision model's context judgments, a
     * dropped step's result replaced by a note.
     */
    def visible: Vector[Step] =
      steps.map: s =>
        if dropped(s.number) && s.outcome.isInstanceOf[Outcome.Succeeded] then s.copy(outcome = Outcome.Succeeded(Json.Str(DroppedNote)))
        else s
    /**
     * The calls made or tried: a step the writer turned into a proposal (no call) doesn't count against the budget, but
     * only the first [[FreeProposals]]: a writer that keeps proposing instead of writing doesn't hold the loop forever.
     */
    def stepsTaken: Int =
      val proposals = steps.count:
        case Step(_, _, _, Outcome.Failed(e)) => e.startsWith(ProposedNote)
        case _                                => false
      steps.size - proposals.min(FreeProposals) - batched
    def unread: Vector[Document] =
      val read = readings.map(_.document).toSet
      documents.filterNot(d => read.contains(d.id))

  private final case class Usage(calls: Int = 0, input: Long = 0, output: Long = 0, latencyMs: Long = 0):
    def +(result: LlmResult[?]): Usage =
      Usage(calls + 1, input + result.inputTokens, output + result.outputTokens, latencyMs + result.latencyMs)
    def ++(other: Usage): Usage =
      Usage(calls + other.calls, input + other.input, output + other.output, latencyMs + other.latencyMs)

  /**
   * Runs the loop for `prompt` over `tools` and the readable `documents`. `context` is what the host knows about the
   * request's environment (the project it's about, say): shown to the decision model with the request, and given to the generative
   * model with it. `live` is read again at every decision: what's changing around the loop as it runs (what other
   * loops working at the same time have done, say), added to the context the decision model and the generative model see.
   */
  def run(
    prompt: String,
    tools: Seq[Tool],
    call: Tools,
    llm: Llm,
    config: Config = Config(),
    documents: Seq[Document] = Nil,
    context: Option[String] = None,
    live: UIO[Sections] = ZIO.succeed(Sections.empty),
  ): ZIO[DecisionModel, Throwable, Result] =
    val names = tools.map(_.name)
    val duplicates = names.diff(names.distinct).distinct
    if tools.size > MaxTools then ZIO.fail(TooManyTools(tools.size))
    else if duplicates.nonEmpty then ZIO.fail(IllegalArgumentException(s"tool names must be unique: ${duplicates.mkString(", ")}"))
    else if config.maxSteps < 0 then ZIO.fail(IllegalArgumentException("maxSteps must not be negative"))
    else
      val initial = State(Vector.empty, Vector.empty, distinct(Vector.empty, documents), Usage(), offered = tools.toVector)
      ZIO.scoped:
       for
        // the other loops' notices for this one (with a bus to hear them on): in its context, and under `Interrupt`
        // stopping its work in flight that one makes out of date
        box      <- Notices.open(prompt, config.notices)
        (heard, calling, writing) = box.fold((live, call, llm))(b => noticed(b, live, call, llm, tools, config))
        filtered <- if config.strategy.relevanceFilter && tools.size > Strategy.FilterAbove then relevance(prompt, context, tools, config)
                    else ZIO.succeed((tools.toVector, None))
        (kept, filterTurn) = filtered
        start     = initial.copy(offered = kept, filtered = kept.size < tools.size)
        done     <- iterate(prompt, project(context), heard, tools, calling, writing, config, start, filterTurn.toVector, 1)
        (reply, ending, state, turns) = done
       yield
        val usage = Decisions.Usage(turns.map(_.usage.inputTokens).sum, turns.map(_.usage.outputTokens).sum)
        Result(reply.value, ending, state.steps, state.readings, turns.toList, usage, state.llm.calls, state.llm.input,
          state.llm.output, state.llm.latencyMs, state.toolLatencyMs, state.dropped)

  /**
   * `live`, `call` and `llm` with an inbox's notices: each decision's context with those since the last, and the calls
   * that change nothing and the arguments' writing in flight, which an urgent notice stops under `Interrupt`.
   */
  private def noticed(box: Notices.Inbox, live: UIO[Sections], call: Tools, llm: Llm, tools: Seq[Tool], config: Config): (UIO[Sections], Tools, Llm) =
    val heard = live.zipWith(box.take.map(Notices.sections))(_ ++ _)
    val inner = call
    val calling = new Tools:
      def call(name: String, arguments: Json.Obj): Task[Json] =
        box.inFlight(s"calling $name ${preview(arguments.toJson, 300)}", tools.find(_.name == name).forall(t => !config.changes(t)))(
          inner.call(name, arguments))
    val writing = new Llm:
      def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]] =
        box.inFlight(s"writing the arguments of ${request.tool.name}", interruptible = true)(llm.arguments(request))
      def reply(request: ReplyRequest): Task[LlmResult[String]] = llm.reply(request)
      override def workflow(request: WorkflowRequest): Task[LlmResult[List[PlannedCall]]] =
        box.inFlight("writing several calls", interruptible = true)(llm.workflow(request))
    (heard, calling, writing)

  private final case class ReplyAbout(request: String, reply: String) derives Schema

  /** Planning, not reporting: told to the reply's writer when the decision model judges its reply reads as the agent's own next steps. */
  val PlanningReply =
    "Your reply read as the agent's own planning (what it would do or call next), not a reply to the user. No more tools " +
      "will run: write to the user what the steps did and found, and what's left, if anything."

  /**
   * `reply`, or, when the decision model judges it the agent's planning rather than a report to the user, a second one written told so
   * (both writings counted). A failed check keeps the reply.
   */
  private def checked(prompt: String, request: ReplyRequest, reply: LlmResult[String], llm: Llm): ZIO[DecisionModel, Throwable, LlmResult[String]] =
    val id = QuestionId("reply_is_planning")
    val question = Question.Noul(
      Content("Is this reply the agent's own planning instead of a reply to the user?").json,
      Decisions.NoulCriteria(
        "it says what the agent will or should do next ('We need to call open_file', 'Let's read the file', 'Next, run " +
          "the tests') as if more tools could run, or it is tool-call JSON, or it's empty of any answer",
        "it reports to the user: what was found or done, the answer, or why it couldn't be done (suggestions for the user " +
          "are fine)",
      ),
    )
    ask(Content(ReplyAbout(prompt.take(2000), reply.value.take(3000))), List(id -> question)).either.flatMap:
      case Right((answers, _, _)) if answers.get(id).collect { case DynamicAnswer.Noul(p) => p.unwrap }.exists(_ >= 0.5) =>
        ZIO.logWarning(s"the reply read as planning; writing it again: ${reply.value.take(120)}") *>
          llm.reply(request.copy(problem = Some(PlanningReply))).map: again =>
            LlmResult(again.value, reply.inputTokens + again.inputTokens, reply.outputTokens + again.outputTokens,
              reply.latencyMs + again.latencyMs)
      case Right(_) => ZIO.succeed(reply)
      case Left(e)  => ZIO.logWarning(s"checking the reply failed: ${e.getMessage}").as(reply)

  private val Unwritten = "the arguments could not be written, so the tool wasn't called"

  private val NextAction = QuestionId("next_action")
  private val AlsoCall = QuestionId("also_call")

  /** One decision model request: its answers, with what it used and how long it took. */
  private[orchestration] def ask(state: Content, questions: List[(QuestionId, Question[?])]): ZIO[DecisionModel, Throwable, (Map[QuestionId, DynamicAnswer], Decisions.Usage, Long)] =
    // a transient failure (a reset connection, throttling, an overloaded or failing server) is retried, five times over
    // about half a minute (concurrent loops reset connections in bursts)
    DecisionModel.decide(state, questions)
      .retry((Schedule.recurs(5) && (Schedule.exponential(1.second) || Schedule.spaced(20.seconds))) &&
        Schedule.recurWhile[Decisions.Error](Decisions.Error.isTransient))
      .timed.map((elapsed, d) => (d.answers, d.usage, elapsed.toMillis))

  /** The loop's steps from `state`: a decision, its actions, and the next, until an ending's reply is written. */
  private def iterate(
    prompt: String,
    base: Sections,
    live: UIO[Sections],
    tools: Seq[Tool],
    call: Tools,
    llm: Llm,
    config: Config,
    state: State,
    turns: Vector[LoopTurn],
    iteration: Int,
  ): ZIO[DecisionModel, Throwable, (LlmResult[String], Ending, State, Vector[LoopTurn])] =
    collect(state).flatMap(state => live.zip(assessNewest(prompt, state, config)).flatMap: (now, assessed) =>
      val (state1, turn) = assessed
      val turns1 = turns ++ turn
      // one decision and its actions; the next decision is the loop's, so a retry of this one (a request too long for
      // The decision model, made again smaller) never replays the steps after it, and the next decision starts from the full budget
      // a loop sent on past its budget (its report says the work isn't done) gets another budget of steps
      val phase = config.copy(maxSteps = config.maxSteps + state1.extraSteps)
      ProcessTree.goal.flatMap(goal => decide(prompt, base, base ++ now, tools, call, llm, phase, state1.copy(goal = goal), turns1, iteration)).flatMap:
        case Left(done) => ZIO.succeed(done)
        case Right((next, all)) if iteration >= 2 * (config.maxSteps + next.extraSteps) + config.maxReadings + 2 + groupCount(tools) =>
          ZIO.fail(IllegalStateException(s"the loop took more than ${2 * (config.maxSteps + next.extraSteps) + config.maxReadings + 2 + groupCount(tools)} decisions"))
        case Right((next, all)) => iterate(prompt, base, live, tools, call, llm, config, next, all, iteration + 1))

  /** The project context the host gave, as the context's first section. */
  private def project(context: Option[String]): Sections =
    context.fold(Sections.empty)(Sections.of("", _, ProjectPriority, ProjectFloor))

  /** The project context's priority among the context's sections, and how much of it is kept before it's dropped. */
  val ProjectPriority = 40
  val ProjectFloor = 2000

  /** How much of a decision request the options may take; past it, their tools' descriptions are cut. */
  val CatalogShare = 0.45

  /**
   * The options of a decision, their tools' descriptions cut until the choice fits [[CatalogShare]] of the request:
   * a large catalog (an IDE's tools, a build server's) otherwise leaves no room for the steps, and the decision model decides without
   * seeing what it has done.
   */
  private def fittedOptions(tools: Seq[Tool], state: State, config: Config, share: Double = CatalogShare,
      boundToo: Boolean = true): Vector[Opt] =
    val target = (config.requestBudget * share).toInt
    def size(opts: Vector[Opt]) = opts.map(o => o.id.length + o.description.json.toJson.length + 4).sum
    def bound(o: Opt) = o.action.isInstanceOf[Action.Bound]
    def at(cap: Int) = options(tools, state, config, cap).filter(o => boundToo || !bound(o))
    // the ready-made calls go last: their arguments are what they are, so they're left out when nothing else fits
    LazyList(Int.MaxValue, 1200, 600, 300, 160, 80).map(at).find(o => size(o) <= target)
      .getOrElse(Some(at(0)).filter(o => size(o) <= target).getOrElse(at(0).filterNot(bound)))

  /** A description cut to about `cap` characters: whole sentences while they fit, else its start. */
  def cutDescription(text: String, cap: Int): String =
    if text.length <= cap then text
    else if cap <= 0 then ""
    else
      val sentences = text.split("(?<=[.!?])\\s+").toList
      val kept = sentences.scanLeft("")((acc, s) => if acc.isEmpty then s else s"$acc $s").tail.takeWhile(_.length <= cap).lastOption
      kept.getOrElse(text.take(cap).trim + "…")

  /** One decision and what it does: the loop's end (a reply), or the state and decisions the next one starts from. */
  private def decide(
    prompt: String,
    base: Sections,
    sections0: Sections,
    tools: Seq[Tool],
    call: Tools,
    llm: Llm,
    config: Config,
    state: State,
    turns: Vector[LoopTurn],
    iteration: Int,
  ): ZIO[DecisionModel, Throwable, Either[(LlmResult[String], Ending, State, Vector[LoopTurn]), (State, Vector[LoopTurn])]] =
    val opts = fittedOptions(tools, state, config)
    val calls = opts.filter(o => o.action.isInstanceOf[Action.Call] || o.action.isInstanceOf[Action.Bound])
    val alsoAsked = config.strategy.parallelCalls && calls.nonEmpty && config.maxSteps - state.stepsTaken >= 2
    // the choice's questions over a set of options (every option's id is one of `opts`'s: a smaller set's are too)
    def choices(of: Vector[Opt]): Task[List[(QuestionId, Question[?])]] =
      val callsOf = of.filter(o => o.action.isInstanceOf[Action.Call] || o.action.isInstanceOf[Action.Bound])
      for
        criteria <- ZIO.fromEither(ChoiceCriteria.fromContent(of.map(o => o.id -> (o.description: Content | Null)).toMap))
                      .mapError(IllegalArgumentException(_))
        also     <- ZIO.fromEither(ChoiceCriteria.fromContent(
                      (callsOf.map(o => o.id -> (o.description: Content | Null)) :+ ("none" -> (Content(NoSecondCall): Content | Null))).toMap,
                    )).mapError(IllegalArgumentException(_))
      yield List(NextAction -> Question.Choice(Content(instructions(config, state)).json, criteria)) ++
        Option.when(alsoAsked && callsOf.nonEmpty)(AlsoCall -> Question.Choice(Content(AlsoInstructions).json, also))
    for
      decisions <- ZIO.environment[DecisionModel]
      chosen0  <- choices(opts)
      judged    = contextQuestions(state, config)
      // a loop that has only read for a while: is it ready to act? (its answer shapes the next decision's options)
      readiness = Option.when(exploring(state, tools, config) >= config.exploreLimit && config.exploreLimit > 0)(
                    ReadyToAct -> Question.Noul(Content(ReadyAbout(exploring(state, tools, config))).json, NoulCriteria(ReadyYes, ReadyNo)))
      // whether the request asks for a change at all: asked once, with the first readiness question
      change    = readiness.filter(_ => state.asksChange.isEmpty).map(_ =>
                    AsksChange -> Question.Noul(Content(AsksChangeAbout).json, NoulCriteria(AsksChangeYes, AsksChangeNo)))
      // every `reviewEvery` calls: did those calls move the request forward? (not on a step the loop is told to act:
      // its reads would be judged as no progress just as it starts the change; the review comes after that step)
      // nor while the loop's reads are judged by readiness (a reading phase is the exploration limit's to end, and its
      // reads found what the change needs however a review would see them)
      review    = Option.when(config.reviewEvery > 0 && !state.halted && state.steps.size - state.reviewedAt >= config.reviewEvery &&
                    !acting(state, tools, config) && readiness.isEmpty)(
                    Reviewing -> Question.Noul(Content(ReviewAbout(state.steps.size - state.reviewedAt)).json, NoulCriteria(ReviewYes, ReviewNo)))
      others    = judged.map(_._2) ++ readiness.toList ++ change.toList ++ review.toList
      questions = chosen0 ++ others
      // a course correction's guidance, with the context, for the decision model and the generative model alike
      sections = withGoal(withRunning(withCorrection(sections0, state, tools, config), state), prompt, state)
      // the options are what they are; the steps and then the context share the rest of the request's budget
      available = config.requestBudget - questionChars(questions) - prompt.length - RequestOverhead
      context   = sections.fit(config.llmContextBudget).renderOption
      _        <- ProcessTree.touch(Some(s"deciding step ${state.stepsTaken + 1}"))
      // a request the decision model finds too long is made again with less of the steps and context, twice: this request only
      // a request too long for the decision model is made again with fewer options, steps and context ([[askFitting]])
      smaller   = (level: Int) => choices(fittedOptions(tools, state, config, CatalogShare / math.pow(2, level), boundToo = level < 2)).map(_ ++ others)
      asked    <- askFitting(prompt, sections, tools, state, config, questions, smaller, available, config.requestBudget, 2)
      (answers, usage, latency) = asked
      answer   <- choice(answers, NextAction)
      chosen   <- ZIO.fromOption(opts.find(_.id == answer.choice)).orElseFail(IllegalStateException(s"The decision model chose an unknown option ${answer.choice}"))
      second    = Option.when(alsoAsked)(answers.get(AlsoCall)).flatten.collect { case DynamicAnswer.Choice(a) => a.choice }
                    .filter(id => id != "none" && id != chosen.id).flatMap(id => calls.find(_.id == id))
                    .filter(_ => chosen.action.isInstanceOf[Action.Call] || chosen.action.isInstanceOf[Action.Bound])
      turn      = LoopTurn(iteration, answer.choice, answer, usage, latency)
      all       = turns :+ turn
      likely    = answer.probabilities.toList.flatMap: (id, p) =>
                    opts.find(_.id == id).toList.flatMap(o => o.action match
                      case Action.Call(tool)        => List(tool.name -> p.unwrap)
                      case Action.Bound(tool, _, _) => List(tool.name -> p.unwrap)
                      case _                        => Nil)
                  .groupMapReduce(_._1)(_._2)(_ max _)
      drops     = judged.flatMap((n, q) => Option.when(!keeps(answers.get(q._1)))(n)).toSet
      readyNow  = readiness.flatMap(_ => answers.get(ReadyToAct)).collect { case DynamicAnswer.Noul(p) => p.unwrap }
      changeNow = change.flatMap(_ => answers.get(AsksChange)).collect { case DynamicAnswer.Noul(p) => p.unwrap }
      _        <- ZIO.foreachDiscard(readyNow)(p => ZIO.logInfo(f"explored ${exploring(state, tools, config)} steps in a row; ready to act: p $p%.2f" +
                    changeNow.fold("")(c => f"; asks for a change: p $c%.2f")) *>
                    note(s"Read for ${exploring(state, tools, config)} steps: making the change next").when(p >= 0.5 && state.ready.forall(_ < 0.5)))
      reviewed  = review.flatMap(_ => answers.get(Reviewing)).collect { case DynamicAnswer.Noul(p) => p.unwrap }
      // judged ready to act in the same decision, the loop acts: its reads were the change's preparation, not a stall
      halt      = reviewed.exists(_ < HaltBelow) && !readyNow.exists(_ >= 0.5)
      _        <- ZIO.foreachDiscard(reviewed)(p => ZIO.logInfo(f"reviewed the last ${state.steps.size - state.reviewedAt} calls: moving forward p $p%.2f" +
                    (if halt then "; ending the loop" else "")) *>
                    note(s"The last ${state.steps.size - state.reviewedAt} calls haven't moved the work forward: ending with what was found").when(halt))
      noted     = state.copy(lastChoice = likely, ready = readyNow.orElse(state.ready), asksChange = changeNow.orElse(state.asksChange),
                    reviewedAt = if review.isDefined then state.steps.size else state.reviewedAt, halted = state.halted || halt)
      result   <- chosen.action match
                    // an ending with an edit no check has run since, and a check to run: the loop goes on to check it, before
                    // any reply is written (a reply that says "I didn't compile it" is written for nothing)
                    case Action.End(_) if config.checkDone && !noted.halted && noted.unfinished.size < MaxUnfinished && unchecked(noted, tools, config).isDefined =>
                      val edit = unchecked(noted, tools, config).get
                      val base = config.maxSteps - noted.extraSteps
                      val more = if config.maxSteps - noted.stepsTaken < 2 then base else 0
                      ZIO.logInfo(s"step ${edit.number}'s ${edit.tool} wasn't checked; going on to check it" +
                          (if more > 0 then s", with another $more steps" else "")) *>
                        note("The change isn't checked yet: checking it") *>
                        ZIO.succeed(Right(noted.copy(extraSteps = noted.extraSteps + more, unfinished = noted.unfinished :+
                          (s"Step ${edit.number} (${edit.tool}) changed the code, and no build or test has run since: the change isn't " +
                            "known to work. Check it now (the project's build or type check, then the tests the change touches), fix " +
                            "what that shows, and end only once a check has passed after the last edit."))))
                    case Action.End(ending) =>
                      // background calls still running are stopped: nothing will read them
                      ZIO.foreachDiscard(noted.running)(_.fiber.interrupt) *> {
                      val actual = if ending == Ending.Answered && noted.stepsTaken >= config.maxSteps then Ending.BudgetExhausted else ending
                      val request = ReplyRequest(prompt, noted.visible, actual, noted.readings, tools, noted.documents, context)
                      note("Writing the reply") *> writer(llm, config, noted, decisions, tools).reply(request)
                        .flatMap(reply => if config.checkReply then checked(prompt, request, reply, writer(llm, config, noted, decisions, tools)) else ZIO.succeed(reply))
                        .flatMap: reply =>
                          // an answer whose own reply says the work isn't done, with steps left: the loop goes on, told so
                          // (out of steps, it gets another budget: what it found is its plan for the next phase)
                          // (not after a call that is the request's whole work: what it started goes on by itself,
                          // and a reply saying so isn't unfinished work of this loop's)
                          val canGoOn = config.checkDone && !noted.halted && noted.unfinished.size < MaxUnfinished && tools.nonEmpty &&
                            !endedByCall(noted, tools, config)
                          (if canGoOn then reportsUnfinished(prompt, reply.value, context) else ZIO.succeed(false)).flatMap:
                            case true =>
                              val base = config.maxSteps - noted.extraSteps
                              val more = if config.maxSteps - noted.stepsTaken < 2 then base else 0
                              ZIO.logInfo(s"going on with another $more steps").when(more > 0).as(
                                Right(noted.copy(llm = noted.llm + reply, unfinished = noted.unfinished :+ reply.value.take(3000),
                                  extraSteps = noted.extraSteps + more)))
                            case false => ZIO.succeed(Left((reply, actual, noted.copy(llm = noted.llm + reply), all))) }
                    case Action.Read(document) => read(document, noted).map(Right(_))
                    case Action.MoreTools      => ZIO.succeed(Right(noted.copy(offered = tools.toVector, filtered = false)))
                    case Action.OpenGroup(g)   => ZIO.logInfo(s"opened the tool group ${g.name}") *> note(s"Looking at the ${g.name} tools") *>
                                                    ZIO.succeed(Right(noted.copy(opened = noted.opened + g.name)))
                    case Action.ReviseGoal     => reviseGoal(prompt, noted, writer(llm, config, noted, decisions, tools), context).map(Right(_))
                    case Action.Move(move)     =>
                      ZIO.logInfo(s"course correction: ${move.id} after step ${noted.steps.size}") *>
                        note(s"Course correction: ${move.description}") *>
                        ZIO.succeed(Right(noted.copy(movedAt = Some(noted.steps.size),
                          correction = if move == Correction.Escalate then noted.correction else Some(move),
                          escalated = noted.escalated || move == Correction.Escalate)))
                    case Action.Background(tool) =>
                      // written now, called in the background; its step comes when it ends
                      val written = writer(llm, config, noted, decisions, tools)
                      runCall(prompt, context, tool, None, noted, 0, call, written, config.strategy.noRepeats, noted.offered).fork.flatMap: fiber =>
                        ZIO.logInfo(s"background: ${tool.name} started after step ${noted.steps.size}")
                          .as(Right(noted.copy(running = noted.running :+ Running(tool, Json.Obj(), noted.steps.size, fiber))))
                    case Action.Await =>
                      // the next decision comes when one ends: no polling
                      ZIO.raceAll(noted.running.head.fiber.await, noted.running.tail.map(_.fiber.await)).as(Right(noted))
                    case Action.Workflow =>
                      runWorkflow(prompt, context, tools, noted, call, writer(llm, config, noted, decisions, tools), config).map(Right(_))
                    case Action.Batch =>
                      runBatch(prompt, context, tools, noted, call, writer(llm, config, noted, decisions, tools), config).map(Right(_))
                    case _ =>
                      runCalls(prompt, context, (chosen.action :: second.map(_.action).toList), noted, call, writer(llm, config, noted, decisions, tools),
                        config.strategy.noRepeats, config.maxParallel).map(Right(_))
    // what the decision model judged no longer needed leaves the projected context from the next decision on
    yield result.map(next => (next.copy(dropped = next.dropped ++ drops), all))

  /**
   * The decision model's answers to `questions`, with the steps and context fitted to `budget`. A request the decision model finds too long (its
   * limit is in tokens, the budgets in characters) is made again, at most `left` times, smaller: three quarters of the
   * budget, and `smaller`'s questions (the options at half their share each time, with no ready-made calls at the
   * last). Still too long with the least there is (no context, every step cut to its start), the loop ends with a
   * reply from what it has, rather than failing the turn: the answer is `end_answer`.
   */
  private def askFitting(prompt: String, sections: Sections, tools: Seq[Tool], state: State, config: Config,
      questions: List[(QuestionId, Question[?])], smaller: Int => Task[List[(QuestionId, Question[?])]], available: Int, budget: Int,
      left: Int, level: Int = 0)
      : ZIO[DecisionModel, Throwable, (Map[QuestionId, DynamicAnswer], Decisions.Usage, Long)] =
    val room = available - (config.requestBudget - budget)
    val fitted = config.copy(stateBudget = config.stateBudget.min((room * 6 / 10).max(OlderPreview * 4)))
    val decisionContext = sections.fit((room - stepsChars(state, fitted)).max(0)).renderOption
    ask(stateView(prompt, decisionContext, tools, state, fitted), questions).catchSome:
      // a refusal (OpenAI's, of content in the state: a result, a document) is asked again with the least state, and
      // if refused still, the loop ends with a reply from what it has, rather than failing the turn
      case _: Decisions.Error.Refused =>
        ZIO.logWarning("The decision model refused the next step; asking again without context, every step cut to " +
          s"$OlderPreview characters") *>
          ask(stateView(prompt, None, tools, state, config.copy(stateBudget = 0, resultPreview = OlderPreview)), questions).catchSome:
            case _: (Decisions.Error.Refused | Decisions.Error.TooLong) =>
              ZIO.logWarning("The decision model refused the next step again; ending the loop with a reply from what it has") *>
                ZIO.succeed((Map(NextAction -> DynamicAnswer.Choice(Decisions.ChoiceAnswer("end_answer", Map.empty, certain))),
                  Decisions.Usage(0, 0), 0L))
      case _: Decisions.Error.TooLong if left > 0 =>
        ZIO.logWarning(s"The decision model's request was too long at a budget of $budget characters (${questionChars(questions)} in its " +
          s"questions); trying ${budget * 3 / 4}, with fewer options") *>
          smaller(level + 1).flatMap: fewer =>
            val freed = questionChars(questions) - questionChars(fewer)
            askFitting(prompt, sections, tools, state, config, fewer, smaller, available + freed, budget * 3 / 4, left - 1, level + 1)
      case _: Decisions.Error.TooLong =>
        ZIO.logWarning(s"The decision model's request was still too long at a budget of $budget characters; trying without context, every " +
          s"step cut to $OlderPreview characters") *>
          ask(stateView(prompt, None, tools, state, config.copy(stateBudget = 0, resultPreview = OlderPreview)), questions).catchSome:
            case _: Decisions.Error.TooLong =>
              ZIO.logWarning("The decision model's request was too long even with the least state; ending the loop with a reply from what it has") *>
                ZIO.succeed((Map(NextAction -> DynamicAnswer.Choice(Decisions.ChoiceAnswer("end_answer", Map.empty, certain))),
                  Decisions.Usage(0, 0), 0L))

  private val certain: Decisions.Probability = Decisions.Probability(1.0).toOption.get

  /** The background calls that ended, as steps, in the order they ended; those still running stay. */
  private def collect(state: State): UIO[State] =
    if state.running.isEmpty then ZIO.succeed(state)
    else ZIO.foreach(state.running)(r => r.fiber.poll.map(r -> _)).map: polled =>
      polled.foldLeft(state.copy(running = Vector.empty)):
        case (s, (r, None)) => s.copy(running = s.running :+ r)
        case (s, (r, Some(exit))) =>
          exit.foldExit(
            _ => s.copy(steps = s.steps :+ Step(s.steps.size + 1, r.tool.name, r.arguments, Outcome.Failed("the background call was stopped"))),
            (step, used, toolMs, _) =>
              val numbered = step.map(_.copy(number = s.steps.size + 1)).map(st =>
                st.copy(outcome = st.outcome match
                  case Outcome.Succeeded(j) => Outcome.Succeeded(Json.Obj("ranInBackgroundFromStep" -> Json.Num(r.afterStep), "result" -> j))
                  case f                    => f))
              s.copy(steps = s.steps ++ numbered, llm = s.llm ++ used, toolLatencyMs = s.toolLatencyMs + toolMs),
          )

  /** The context with what's running in the background, for the decision model and the generative model. */
  private def withRunning(sections: Sections, state: State): Sections =
    if state.running.isEmpty then sections
    else Sections.of("Running in the background (its result comes as a step when it ends; choose wait when there's nothing else to do)",
      state.running.map(r => s"- ${r.tool.name}, started after step ${r.afterStep}").mkString("\n"), 85, Int.MaxValue) ++ sections

  private final case class BackgroundOption(action: String, tool: String, description: String) derives Schema

  private final case class WorkflowOption(action: String, description: String) derives Schema

  private val WorkflowDescription =
    "Do several calls in one go: the generative model writes a short sequence of calls (say, an edit, then the build, " +
      "then the one test), which run in order, each a step, stopping at the first that fails. Best when the next few " +
      "calls are clear and each follows from the one before; one writing and one decision instead of several."

  private val ReviseGoalDescription =
    "Change the goal this work is toward, when the work has shown it should: narrow it to what is actually needed, " +
      "correct it (the request meant something else, as the steps show), or drop it (it's already done, done another " +
      "way, or the user no longer wants it). No tool is called; the next decisions work toward the revised goal, and a " +
      "dropped goal ends the work. Don't choose it to make the work easier than what was asked."

  private val BatchDescription =
    "Do several independent things at once: the generative model writes a batch of calls of any of the tools (files to " +
      "read, pages to fetch, searches, edits of different files, commands) that don't depend on each other's results, and " +
      "they all run at the same time, each a step. Best when several things are already known to be needed (the files a " +
      "listing or search found, the pages a spec links to, the same change in several files): one decision and one " +
      "writing instead of one for each, and the whole batch takes one step of the budget."

  /** The tools a batch may call: the offered ones but those that end the turn, and the reading ones once told to act. */
  private def batchable(tools: Seq[Tool], state: State, config: Config): Vector[Tool] =
    val act = acting(state, tools, config)
    state.offered.filterNot(t => config.endsTurn(t) || (act && config.explores(t)))

  /**
   * A batch: the calls the generative model writes (at most [[Config.maxParallel]]), each a step, all run at the same
   * time through [[runCalls]] ([[Work]]); a call that already succeeded isn't made again. A call of a tool not offered,
   * or whose arguments don't fit its schema, is a failed step of its own; the others still run. None written is a failed
   * step. The batch takes one step of the budget, however many calls it has.
   */
  private def runBatch(prompt: String, context: Option[String], tools: Seq[Tool], state: State, call: Tools, llm: Llm,
      config: Config): UIO[State] =
    val usable = batchable(tools, state, config)
    val max = config.maxParallel.max(1)
    llm.workflow(WorkflowRequest(prompt, usable, state.visible, state.readings, context, max, concurrent = true)).either.flatMap:
      case Left(error) =>
        ZIO.succeed(state.copy(steps = state.steps :+ Step(state.steps.size + 1, "batch", Json.Obj(),
          Outcome.Failed(s"$Unwritten: no batch: ${message(error)}"))))
      case Right(written) =>
        val planned = written.value.distinctBy(p => (p.tool, p.arguments.toJson)).take(max)
        // what can't be called is a failed step, numbered after the calls that run
        val (runnable, refused) = planned.partitionMap: p =>
          usable.find(_.name == p.tool) match
            case None => Right(p -> s"not called: ${p.tool} isn't one of the tools this batch can call (${usable.map(_.name).mkString(", ")})")
            case Some(tool) => misfit(tool.inputSchema, p.arguments) match
              case Some(problem) => Right(p -> s"not called: the arguments don't fit its schema: $problem")
              case None          => Left(Action.Bound(tool, p.arguments, state.steps.size))
        val used = state.copy(llm = state.llm + written)
        ZIO.logInfo(s"batch: ${planned.size} calls at once: ${planned.map(_.tool).mkString(", ")}") *>
          note(s"Doing ${planned.size} things at once") *>
          runCalls(prompt, context, runnable, used, call, llm, config.strategy.noRepeats, config.maxParallel).map: ran =>
            val all = ran.steps ++ refused.zipWithIndex.map { case ((p, why), i) =>
              Step(ran.steps.size + 1 + i, p.tool, p.arguments, Outcome.Failed(why)) }
            // the batch is one step of the budget
            ran.copy(steps = all, batched = ran.batched + (all.size - state.steps.size - 1).max(0))

  /**
   * A workflow: the calls the generative model writes (at most [[Strategy.MaxWorkflowCalls]] and the steps left), run in
   * order with their written arguments, each a step, stopping at the first that fails. A call of a tool not offered, or
   * whose arguments don't fit its schema, is a failed step and ends the workflow. No workflow written is a failed step.
   */
  private def runWorkflow(prompt: String, context: Option[String], tools: Seq[Tool], state: State, call: Tools, llm: Llm,
      config: Config): UIO[State] =
    val max = Strategy.MaxWorkflowCalls.min(config.maxSteps - state.stepsTaken).max(1)
    llm.workflow(WorkflowRequest(prompt, state.offered, state.visible, state.readings, context, max)).either.flatMap:
      case Left(error) =>
        ZIO.succeed(state.copy(steps = state.steps :+ Step(state.steps.size + 1, "workflow", Json.Obj(),
          Outcome.Failed(s"$Unwritten: no workflow: ${message(error)}"))))
      case Right(written) =>
        val used = state.llm + written
        ZIO.logInfo(s"workflow: ${written.value.map(_.tool).mkString(" → ")}") *>
          ZIO.foldLeft(written.value.take(max))((state.copy(llm = used), false)):
            case ((s, true), _) => ZIO.succeed((s, true))
            case ((s, false), planned) =>
              val number = s.steps.size + 1
              s.offered.find(_.name == planned.tool) match
                case None =>
                  ZIO.succeed((s.copy(steps = s.steps :+ Step(number, planned.tool, planned.arguments,
                    Outcome.Failed(s"not a tool this loop has: ${planned.tool}"))), true))
                case Some(tool) =>
                  misfit(tool.inputSchema, planned.arguments) match
                    case Some(problem) =>
                      ZIO.succeed((s.copy(steps = s.steps :+ Step(number, tool.name, planned.arguments,
                        Outcome.Failed(s"not called: the arguments don't fit its schema: $problem"))), true))
                    case None =>
                      ProcessTree.run(ProcessTree.Kind.Call, tool.name, Some(preview(planned.arguments.toJson, 200)))(call.call(tool.name, planned.arguments))
                        .fold(e => Outcome.Failed(message(e)), Outcome.Succeeded(_)).timed.map: (elapsed, outcome) =>
                          val step = Step(number, tool.name, planned.arguments, outcome)
                          (s.copy(steps = s.steps :+ step, toolLatencyMs = s.toolLatencyMs + elapsed.toMillis), outcome.isInstanceOf[Outcome.Failed])
          .map(_._1)

  /** The generative model that writes: the stronger one once the decision model escalated, when there is one. */
  private def writer(llm: Llm, config: Config, state: State, decisions: ZEnvironment[DecisionModel], tools: Seq[Tool]): Llm =
    val chosen = if state.escalated then config.escalation.getOrElse(llm) else llm
    if config.briefArguments then Briefed(chosen, decisions, config, tools) else chosen

  /**
   * The arguments' writer, given all the work so far, still lacks something it needs (a file not read, a definition
   * not looked up): the call isn't made, and the step says what to get first.
   */
  final case class NeedsMore(tool: String, what: String)
      extends RuntimeException(s"the arguments' writer of $tool needs what the work hasn't found yet: $what")

  /** The property an arguments' writer sets, instead of guessing, when what it was given isn't enough ([[Briefed]]). */
  val AskForMore = "need_more_information"

  /**
   * `llm` with its arguments' requests briefed ([[Config.briefArguments]]): before a call's arguments are written, the
   * decision model is asked, of each earlier step and each document read, whether writing this call's arguments needs it
   * (one yes-or-no question each), and the same of the project context in a request of its own, at the same time (a
   * refused context is kept, without losing the steps' briefing); the writer gets the request, the tool and only those (and this
   * tool's own failed calls, always: what not to write again; an edit's, what the reads found). The tool's schema gains
   * an optional [[AskForMore]]: a writer that can't write the arguments from what it was given says what's missing, and
   * is asked again with everything; one that still can't (what it needs isn't in the work yet: a file not read, a
   * definition not looked up) fails with [[NeedsMore]], a step that tells the decision model what to get first. The replies,
   * workflows and batches are written as they were.
   */
  private final class Briefed(llm: Llm, decisions: ZEnvironment[DecisionModel], config: Config, tools: Seq[Tool]) extends Llm:
    def reply(request: ReplyRequest): Task[LlmResult[String]] = llm.reply(request)
    override def workflow(request: WorkflowRequest): Task[LlmResult[List[PlannedCall]]] = llm.workflow(request)

    def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]] =
      val full = request.copy(tool = request.tool.copy(inputSchema = withAsk(request.tool.inputSchema)))
      brief(full).provideEnvironment(decisions).catchAll(e =>
        ZIO.logWarning(s"briefing the arguments of ${request.tool.name} failed (${message(e)}); writing them with everything").as(full))
      .flatMap: briefed =>
        llm.arguments(briefed).flatMap: written =>
          asked(written) match
            case None => ZIO.succeed(stripped(written))
            case Some(what) if briefed == full =>
              // it had everything: what it needs isn't in the work yet, so the agent gets it first
              needsMore(request, what)
            case Some(what) =>
              ZIO.logInfo(s"the arguments' writer of ${request.tool.name} asked for more: ${preview(what, 300)}") *>
                note(s"Giving the writer more to go on for ${request.tool.name}") *>
                llm.arguments(full.copy(problem = Some(
                    s"You were given only part of the work so far and said you needed: $what. Here is all of it. If " +
                      s"what you need still isn't here, set $AskForMore again, saying what to read or look up." +
                      request.problem.fold("")(p => s"\n\n$p"))))
                  .map(again => again.copy(inputTokens = again.inputTokens + written.inputTokens,
                    outputTokens = again.outputTokens + written.outputTokens, latencyMs = again.latencyMs + written.latencyMs))
                  .flatMap(again => asked(again).fold(ZIO.succeed(stripped(again)))(needsMore(request, _)))

    private def needsMore(request: ArgumentsRequest, what: String): IO[NeedsMore, Nothing] =
      ZIO.logInfo(s"the arguments' writer of ${request.tool.name} needs more than the work has: ${preview(what, 300)}") *>
        ZIO.fail(NeedsMore(request.tool.name, what))

    /** What the writer said it needs, when it set [[AskForMore]]. */
    private def asked(written: LlmResult[Json.Obj]): Option[String] =
      written.value.get(AskForMore).map(a => a.asString.map(_.trim).getOrElse(a.toJson)).filter(_.nonEmpty)

    private def stripped(written: LlmResult[Json.Obj]): LlmResult[Json.Obj] =
      written.copy(value = Json.Obj(written.value.fields.filterNot(_._1 == AskForMore)*))

    /**
     * The request with only what the decision model judges writing this call needs; as it is when there's nothing to choose from. An
     * edit's writer always gets what the reads found (an edit copies the lines it replaces, which the decision model can't judge from
     * a preview), and every writer this tool's own failures.
     */
    private def brief(request: ArgumentsRequest): ZIO[DecisionModel, Throwable, ArgumentsRequest] =
      val change = config.changes(request.tool)
      def read(s: Step) = s.outcome.isInstanceOf[Outcome.Succeeded] && tools.find(_.name == s.tool).exists(config.explores)
      val own = request.failures.map(_.number).toSet ++ request.steps.filter(s => change && read(s)).map(_.number)
      val steps = request.steps.filterNot(s => own(s.number))
      if steps.isEmpty && request.readings.isEmpty && request.context.isEmpty then ZIO.succeed(request)
      else
        val criteria = NoulCriteria(
          s"writing the arguments of ${request.tool.name} needs it: it holds a value an argument takes (a path, a name, " +
            "an id, a line to replace, an error saying what a valid call looks like)",
          s"the arguments of ${request.tool.name} can be written without it: nothing in it goes into them",
        )
        def question(id: String, about: Json) = QuestionId(id) -> Question.Noul(
          Content(BriefAbout(s"Does writing the arguments of ${request.tool.name} need this?", about)).json, criteria)
        val items =
          steps.toList.map(s => question(f"step_${s.number}%03d", Json.Obj(
            "step" -> Json.Num(s.number), "tool" -> Json.Str(s.tool), "arguments" -> s.arguments,
            "outcome" -> Json.Str(preview(s.outcome match
              case Outcome.Succeeded(r) => r.toJson
              case Outcome.Failed(e)    => s"failed: $e", 600))))) ++
          request.readings.toList.map(r => question(f"read_${r.number}%03d", Json.Obj(
            "document" -> Json.Str(r.title), "text" -> Json.Str(preview(r.text.fold(identity, identity), 600)))))
        val contextQuestion =
          request.context.toList.map(c => question("context", Json.Obj("projectContext" -> Json.Str(preview(c, 1500)))))
        val about = Content(BriefState(request.prompt.take(4000), request.tool.name, preview(request.tool.description, 600),
          parameters(request.tool.inputSchema)))
        val itemsAsked =
          if items.isEmpty then ZIO.none else ask(about, items).map((answers, _, _) => Some(answers))
        // the project context is asked apart, at the same time: the decision model refuses some contexts, and a refusal
        // keeps the context instead of failing the briefing of every step and document
        val contextAsked =
          if contextQuestion.isEmpty then ZIO.none else ask(about, contextQuestion).map((answers, _, _) => Some(answers))
        itemsAsked.zipPar(contextAsked.catchAll(e =>
            ZIO.logInfo(s"briefing the project context for ${request.tool.name} failed (${message(e)}); keeping it").as(None)))
          .map: (itemAnswers, contextAnswers) =>
            val answers = itemAnswers.toList ++ contextAnswers.toList
            def kept(id: String) = answers.flatMap(_.get(QuestionId(id))).headOption
              .collect { case DynamicAnswer.Noul(p) => p.unwrap >= BriefThreshold }.getOrElse(true)
            val keptSteps = request.steps.filter(s => own(s.number) || kept(f"step_${s.number}%03d"))
            val keptReadings = request.readings.filter(r => kept(f"read_${r.number}%03d"))
            val keptContext = request.context.filter(_ => kept("context"))
            request.copy(steps = keptSteps, readings = keptReadings, context = keptContext)
        .tap(b => ZIO.logInfo(s"arguments of ${request.tool.name} briefed: ${b.steps.size} of ${request.steps.size} steps, " +
          s"${b.readings.size} of ${request.readings.size} documents, ${if b.context.isDefined then "the" else "no"} project context"))

  /** At or above this, the decision model's judgment that writing a call needs a step (or document, or the context) keeps it. */
  val BriefThreshold = 0.4

  private final case class BriefAbout(question: String, item: Json) derives Schema
  private final case class BriefState(request: String, tool: String, description: String, parameters: List[String]) derives Schema

  /** `schema` with the [[AskForMore]] property: optional, a string. */
  private def withAsk(schema: Json.Obj): Json.Obj =
    val props = schema.get("properties").flatMap(_.asObject).getOrElse(Json.Obj())
    val ask = Json.Obj("type" -> Json.Str("string"), "description" -> Json.Str(
      "Only when what you were given isn't enough to write correct arguments (a value you'd have to guess): say what's " +
        "missing here, and you'll be given everything. Leave it out otherwise."))
    Json.Obj(schema.fields.filterNot(_._1 == "properties") :+ ("properties" -> Json.Obj((props.fields :+ (AskForMore -> ask))*))*)

  /**
   * The context with the loop's goal in front when it isn't the request as written (the user steered it, the decision
   * model revised it): what the work is now toward, which every decision and writing sees.
   */
  private def withGoal(sections: Sections, prompt: String, state: State): Sections =
    state.goal.filter(_.trim != prompt.trim).fold(sections)(g =>
      Sections.of("The goal now (it has changed since the request; work toward this)", g, 97, 3000) ++ sections)

  /** How many times a loop may revise its goal: a goal that keeps changing isn't one. */
  val MaxRevisions = 3

  /** What the generative model writes to revise a goal: the new goal, or that it's dropped, and why. */
  private val ReviseGoalTool = Tool("revise_goal",
    "Revises the goal the agent works toward, from what the work has shown: a narrower or corrected goal, or the goal " +
      "dropped (it's already done, done another way, or no longer wanted). No tool is called.",
    Json.Obj("type" -> Json.Str("object"), "required" -> Json.Arr(Json.Str("why")), "properties" -> Json.Obj(
      "goal" -> Json.Obj("type" -> Json.Str("string"), "description" -> Json.Str("The goal as it should now be, in full; leave it out to drop the goal")),
      "drop" -> Json.Obj("type" -> Json.Str("boolean"), "description" -> Json.Str("True to drop the goal: nothing is left to do toward it")),
      "why" -> Json.Obj("type" -> Json.Str("string"), "description" -> Json.Str("What in the work shows the goal should change")))))

  /**
   * The decision model chose to revise the loop's goal: the generative model writes the new one (or that it's dropped,
   * and why), and the loop's node in the process tree gets it. A dropped goal ends the loop: only the endings are
   * offered next. Not a step of the budget.
   */
  private def reviseGoal(prompt: String, state: State, llm: Llm, context: Option[String]): UIO[State] =
    note("Revising the goal") *>
    llm.arguments(ArgumentsRequest(prompt, ReviseGoalTool, state.visible, state.readings, context = context)).either.flatMap:
      case Left(e) =>
        ZIO.logWarning(s"the goal couldn't be revised: ${message(e)}").as(state.copy(revisions = state.revisions + 1))
      case Right(written) =>
        val o = written.value
        val why = o.get("why").flatMap(_.asString).map(_.trim).filter(_.nonEmpty).getOrElse("the work showed it")
        val goal = o.get("goal").flatMap(_.asString).map(_.trim).filter(_.nonEmpty)
        val drop = o.get("drop").flatMap(_.asBoolean).contains(true) || goal.isEmpty
        val used = state.copy(llm = state.llm + written, revisions = state.revisions + 1)
        if drop then
          ZIO.logInfo(s"goal dropped: $why") *> note(s"The goal is dropped: $why") *>
            ProcessTree.removeGoal(s"the decision model dropped it: $why").as(used.copy(goal = None, halted = true))
        else
          ZIO.logInfo(s"goal revised: ${preview(goal.get, 300)} ($why)") *> note(s"The goal is now: ${preview(goal.get, 300)}") *>
            ProcessTree.setGoal(goal.get, s"the decision model revised it: $why").as(used.copy(goal = goal))

  /** The context with a course correction's guidance in front: the highest priority, never cut. */
  private def withCorrection(sections: Sections, state: State, tools: Seq[Tool], config: Config): Sections =
    val guided = if acting(state, tools, config) then Sections.of("Enough reading: make the change", ActGuidance(exploring(state, tools, config)), 94, Int.MaxValue) ++ sections
                 else sections
    val corrected = state.correction.fold(guided)(c => Sections.of("", c.guidance, 95, Int.MaxValue) ++ guided)
    state.unfinished.lastOption.fold(corrected): report =>
      Sections.of("The work isn't done", "The agent was about to end, but its own report says the request isn't done yet. " +
        "Go on with what it says is missing (find what wasn't found, make the change that wasn't made, check it); end only " +
        s"when it's done or can't be. Its report:\n$report", 96, 1500) ++ corrected

  /** How many steps in a row, the newest last, called a tool that only explores. */
  private def exploring(state: State, tools: Seq[Tool], config: Config): Int =
    state.steps.reverseIterator.takeWhile(s => tools.find(_.name == s.tool).exists(config.explores)).size

  /**
   * Whether the loop is told to act: it has explored [[Config.exploreLimit]] steps in a row, there's a tool that does
   * more than explore, and the decision model judged it ready, or it has read [[ExploreCap]] times the limit, or the request asks for
   * a change and only [[ActReserve]] steps are left: a loop that reads until its budget is spent ends with nothing
   * done, the commonest way a change request fails.
   */
  private def acting(state: State, tools: Seq[Tool], config: Config): Boolean =
    val explored = exploring(state, tools, config)
    val left = config.maxSteps - state.stepsTaken
    config.exploreLimit > 0 && tools.exists(t => !config.explores(t)) && explored >= config.exploreLimit &&
      (explored >= config.exploreLimit * ExploreCap || state.ready.exists(_ >= 0.5) ||
        (left <= ActReserve && state.asksChange.exists(_ >= 0.5)))

  /** How many times [[Config.exploreLimit]] a loop reads in a row before it's told to act, ready or not. */
  val ExploreCap = 2

  /** The steps kept for acting: with this many left, a loop that has only read is told to make the change. */
  val ActReserve = 3

  private val ReadyToAct = QuestionId("ready_to_act")
  private val AsksChange = QuestionId("asks_change")

  private val AsksChangeAbout = "Does the user's request need the agent to do more than read: change something (code, docs, " +
    "config, files), or run something (a test, a measurement, a command, a script) to produce what it asks for?"
  private val AsksChangeYes = "the request asks for a change (something added, fixed, implemented, edited or removed), or for " +
    "something only running can give: numbers (how long, how much, how often, where the time goes), a reproduction, a test " +
    "written and run, a result checked"
  private val AsksChangeNo = "the request only asks a question about the code, for an explanation or a review, answerable by " +
    "reading: nothing is to be changed or run"

  private final case class ReadyAbout(question: String, explored: Int) derives Schema
  private object ReadyAbout:
    def apply(explored: Int): ReadyAbout = ReadyAbout(
      "The agent has only read (files, searches, pages) for the last steps. Has it read enough to start the work the " +
        "request asks for, so the next step can be the first edit, or the first run of what produces the request's answer " +
        "(a test, a command, a measurement)?", explored)

  private val ReadyYes = "the request asks for a change or for something only running can give (numbers, a reproduction, " +
    "a test run), and what's been read is enough to start it: the files to change, or what to run or measure, are known; " +
    "anything still unknown can be learned by making the change, or running it, and checking"
  private val ReadyNo = "the request asks for nothing to be changed or run (a question about the code, an explanation), " +
    "or a specific fact the first edit or run needs (a file not yet found, an API not yet seen) is still missing and the " +
    "next read would find it; reading files again, or reading around the code already found, doesn't count"

  private def ActGuidance(explored: Int): String =
    s"The agent has read for $explored steps in a row without changing or running anything, and has read enough to " +
      "start. Make the change now, or run what the request needs: the next step edits or writes a file (a first, small " +
      "part of the change is fine), or runs something (a test, a command, a measurement, a script over existing output) " +
      "that makes progress; reading tools aren't offered until it does. What's still unknown comes from making the change " +
      "or running it and looking at the result, not from more reading."

  /**
   * The last edit no check has run since, when there's a check the loop can run: the step that changed something (not
   * a read, not a check) and succeeded, after the last check that succeeded. None when the loop changed nothing, or
   * checked after its last change, or has no check to run.
   */
  private def unchecked(state: State, tools: Seq[Tool], config: Config): Option[Step] =
    def tool(s: Step) = tools.find(_.name == s.tool)
    def ok(s: Step) = s.outcome.isInstanceOf[Outcome.Succeeded]
    val canCheck = state.offered.exists(config.isCheck)
    val lastEdit = state.steps.filter(s => ok(s) && tool(s).exists(t => config.changes(t) && !config.explores(t) && !config.isCheck(t) && !config.endsTurn(t))).lastOption
    val lastCheck = state.steps.filter(s => ok(s) && tool(s).exists(config.isCheck)).lastOption.map(_.number).getOrElse(0)
    lastEdit.filter(e => canCheck && e.number > lastCheck)

  private val Reviewing = QuestionId("work_moving_forward")

  /** Below this, the decision model's judgment that the last calls moved the work forward ends the loop ([[Config.reviewEvery]]). */
  val HaltBelow = 0.2

  private final case class ReviewAbout(question: String, calls: Int) derives Schema
  private object ReviewAbout:
    def apply(calls: Int): ReviewAbout = ReviewAbout(
      s"Look at the agent's last $calls calls against the request. Did they move the request forward?", calls)

  private val ReviewYes = "they found something the request needs (a file, a cause, a fact), changed something it asks for, " +
    "or a check's result changed (fewer failures, a new error further on); work that is slow but getting somewhere counts"
  private val ReviewNo = "they learned nothing new: the same reads, searches or failing checks again, calls that fail the same " +
    "way, or circling without a result; or the work is blocked on something only the user can give (a login, a credential, " +
    "an approval, a choice)"

  /** How many times a loop's answer is turned back as unfinished before it's let end. */
  val MaxUnfinished = 2

  private final case class ReplyReport(request: String, reply: String, earlierWork: Option[String]) derives Schema

  /** the decision model's judgment that `reply` (the agent's report) says the request isn't done; a failed judgment lets the loop end. */
  /**
   * `context`'s earlier work (a follow-up's "go ahead" is the work an earlier task left), so the request's scope is
   * judged, not only its words.
   */
  private def reportsUnfinished(prompt: String, reply: String, context: Option[String] = None): ZIO[DecisionModel, Nothing, Boolean] =
    val earlier = context.flatMap: c =>
      val i = c.indexOf("Earlier work")
      Option.when(i >= 0)(c.substring(i).take(4000))
    val id = QuestionId("reply_says_unfinished")
    ask(Content(ReplyReport(prompt.take(2000), reply.take(3000), earlier)), List(id -> Question.Noul(
      Content("Does this report leave the user's request itself undone (the change it asks for not made, the question it asks not answered)?").json,
      Decisions.NoulCriteria(
        "the request's own work isn't there yet: the change it asks for wasn't made, or was made but not built or tested " +
          "(\"I didn't compile it\", \"not verified\"), or the report lists parts of the request still missing (tests the " +
          "change needs, a piece not wired in); or what it asks to find wasn't found; or a question about the code is " +
          "answered without reading that code, as the report itself says (\"I didn't read the sources\", \"as documented, " +
          "not the implementation\")",
        "the report does what the request asks: it answers the question (a \"no\", \"there isn't one\" or \"it doesn't " +
          "exist\" answers a question), reports the change made and checked, says why it can't be done, or asks the user " +
          "for what only they can give (a login, a credential, an approval, a choice between approaches); other work it " +
          "mentions (other features, next steps it suggests) doesn't make this request undone, but what a follow-up's " +
          "earlier work said the request includes does",
      ),
    ))).map((answers, _, _) => answers.get(id).collect { case DynamicAnswer.Noul(p) => p.unwrap }.exists(_ >= 0.5))
      .tap(u => (ZIO.logInfo("the answer reports the work unfinished; going on") *>
        note("The reply says the work isn't done: going on with it")).when(u))
      .catchAll(e => ZIO.logWarning(s"checking whether the work is done failed: ${e.getMessage}").as(false))

  private final case class AssessAbout(question: String, step: Int, tool: String, arguments: String, outcome: String) derives Schema

  private val Worked = QuestionId("newest_worked")
  private val Repeats = QuestionId("newest_repeats")
  private val Informative = QuestionId("newest_informative")
  private val Progressing = QuestionId("work_progressing")

  private final case class ProgressAbout(question: String, failedChecks: List[String]) derives Schema

  /**
   * [[Strategy.correct]]'s questions about the newest step not yet assessed: did it work, does its failure repeat an
   * earlier one (a choice among the earlier steps of the same tool, or none), and does it say why. Asked in the
   * decision's own request: the decision model answers them with the next action, at no extra round trip.
   */
  private def assessmentQuestions(state: State, config: Config): List[(Int, (QuestionId, Question[?]))] =
    def check(s: Step) = state.offered.find(_.name == s.tool).exists(config.isCheck)
    if !config.strategy.correct then Nil
    else state.steps.lastOption.filterNot(s => state.assessed.contains(s.number)).filter(check).toList.flatMap: newest =>
      val outcome = newest.outcome match
        case Outcome.Succeeded(r) => s"succeeded: ${preview(r.toJson, 3000)}"
        case Outcome.Failed(e)    => s"failed: ${preview(e, 3000)}"
      val about = AssessAbout("", newest.number, newest.tool, preview(newest.arguments.toJson, 400), outcome)
      val earlier = state.steps.dropRight(1).filter(s => s.tool == newest.tool && state.assessed.get(s.number).exists(_.failed)).takeRight(8)
      val worked = Worked -> Question.Noul(
        Content(about.copy(question = "This step checks the work. Does it show the work working? A call can succeed and " +
          "still show it not working: tests that fail or time out, a build with errors, a command with a non-zero exit.")).json,
        NoulCriteria("the work works: the tests it ran passed, the build is clean, the command did what it should",
          "it doesn't: a test failed or timed out, the build has errors, the command failed"))
      val repeats = Option.when(earlier.nonEmpty):
        Repeats -> Question.Choice(
          Content(about.copy(question = "If this step failed: does its failure repeat an earlier step's failure (the same " +
            "test failing the same way, the same error at the same place), even though code was changed in between?")).json,
          Decisions.ChoiceCriteria.fromContent((("none" -> (Content("No: it didn't fail, or it fails differently from every earlier step"): Content | Null)) ::
            earlier.toList.map(e => f"step_${e.number}%03d" -> (Content(AssessAbout("the earlier step", e.number, e.tool,
              preview(e.arguments.toJson, 200), e.outcome match
                case Outcome.Succeeded(r) => s"succeeded: ${preview(r.toJson, 800)}"
                case Outcome.Failed(m)    => s"failed: ${preview(m, 800)}")): Content | Null))).toMap).toOption.get)
      val informative = Informative -> Question.Noul(
        Content(about.copy(question = "If this step failed: does its result say what's wrong (an assertion with expected " +
          "and actual values, an error message naming the cause, a stack trace into the code), not only that something " +
          "failed or timed out?")).json,
        NoulCriteria("it shows the cause, or the step didn't fail", "it says only that it failed, timed out or hung"))
      val failedBefore = state.steps.filter(s => state.assessed.get(s.number).exists(_.failed))
      val progressing = Option.when(config.strategy.giveUp && failedBefore.size + 1 >= Strategy.GiveUpAfter):
        Progressing -> Question.Noul(
          Content(ProgressAbout("If this check failed too: across these failed checks, in order, is the work getting closer " +
            "to working (fewer failures, different or more specific errors, a failure further along), so that another attempt " +
            "is likely to fix it?", (failedBefore :+ newest).toList.map(s => s"step ${s.number} ${s.tool}: " + (s.outcome match
              case Outcome.Succeeded(r) => preview(r.toJson, 1200)
              case Outcome.Failed(e)    => preview(e, 1200))))).json,
          NoulCriteria("it's getting closer: the failures change, shrink or get more specific",
            "it isn't: the same failure, in the same way, attempt after attempt"))
      (worked :: informative :: (repeats.toList ++ progressing.toList)).map(newest.number -> _)

  /**
   * [[Strategy.correct]]: the newest step assessed by the decision model, in a request of its own before the decision (The decision model is fast:
   * a fraction of a second), so the decision already knows whether it worked and can be offered the moves. A failed
   * request leaves the step unassessed.
   */
  private def assessNewest(prompt: String, state: State, config: Config): URIO[DecisionModel, (State, Option[LoopTurn])] =
    val questions = assessmentQuestions(state, config)
    questions.headOption.map(_._1) match
      case None => ZIO.succeed((state, None))
      case Some(step) =>
        ask(Content(AssessView(prompt)), questions.map(_._2)).map: (answers, usage, latency) =>
          val next = recordAssessment(state, answers, Some(step))
          val a = next.assessed(step)
          val answer = ChoiceAnswer(if a.stuck then "stuck" else if a.failed then "failed" else "worked", Map.empty, Probability(a.worked).toOption.get)
          (next, Some(LoopTurn(0, s"assess step $step", answer, usage, latency)))
        .catchAll(e => ZIO.logWarning(s"couldn't assess step $step: ${message(e)}").as((state, None)))

  private final case class AssessView(request: String) derives Schema

  /** The decision's assessment of the step it was asked about, kept; a step that works ends a course correction. */
  private def recordAssessment(state: State, answers: Map[QuestionId, DynamicAnswer], step: Option[Int]): State =
    step.fold(state): n =>
      val worked = answers.get(Worked).collect { case DynamicAnswer.Noul(p) => p.unwrap }.getOrElse(1.0)
      val informative = answers.get(Informative).collect { case DynamicAnswer.Noul(p) => p.unwrap }.getOrElse(1.0)
      val repeats = answers.get(Repeats).collect { case DynamicAnswer.Choice(a) => a.choice }
        .collect { case s"step_$k" => k.toIntOption }.flatten
      val progressing = answers.get(Progressing).collect { case DynamicAnswer.Noul(p) => p.unwrap }
      val a = Assessment(worked, repeats.filter(_ => worked < 0.5), informative, progressing)
      // only a check is assessed: one that shows the work working ends the correction
      state.copy(assessed = state.assessed.updated(n, a), correction = if a.failed then state.correction else None)

  private final case class MoveOption(action: String, move: String, description: String) derives Schema

  /** The moves offered after a step that didn't work, once per such step; escalation only when there's a model to escalate to. */
  private def moves(state: State, config: Config): Vector[Opt] =
    val newest = state.steps.lastOption
    // after a check that failed, once; with a move active, only when a check made since it failed again
    val failedCheck = newest.exists(s => state.assessed.get(s.number).exists(_.failed))
    val sinceMove = state.movedAt.forall(m => newest.exists(_.number > m))
    val due = config.strategy.correct && failedCheck && !state.movedAt.contains(state.steps.size) && sinceMove
    if !due then Vector.empty
    else Correction.values.toVector
      .filter(m => m != Correction.Escalate || (config.escalation.isDefined && !state.escalated))
      .filterNot(m => state.correction.contains(m))
      .map(m => Opt(s"move_${m.id}", Action.Move(m), Content(MoveOption("change_course", m.id, m.description))))

  /** What a decision request has besides the state and the questions (its envelope, field names), in characters. */
  private val RequestOverhead = 2000

  /** How many characters `questions` take in a decision request: the request's own rendering, with an empty state. */
  private def questionChars(questions: List[(QuestionId, Question[?])]): Int =
    com.jamesward.zio_decisions.internal.Canonical.request(new Content(Json.Obj()), "", questions).toJson.length

  /** How many characters the steps take in the decision model's state under `config`'s state budget. */
  private def stepsChars(state: State, config: Config): Int =
    Content(budgeted(state.steps, config, state.dropped)).json.toJson.length

  private val DroppedNote = "(result left out of context: judged no longer needed)"

  private final case class KeepAbout(question: String, step: Int, tool: String, arguments: String, result: String) derives Schema

  /** The steps to judge (earlier ones still in context, with long results; not the newest) and their questions. */
  private def contextQuestions(state: State, config: Config): List[(Int, (QuestionId, Question[?]))] =
    val mode = config.strategy.context
    if mode == ContextChoice.All then Nil
    else
      state.steps.dropRight(1).toList.collect:
        case Step(n, tool, arguments, Outcome.Succeeded(result)) if !state.dropped(n) && result.toJson.length >= Strategy.KeepShorterThan =>
          val about = Content(KeepAbout(
            "Is this step's result still needed in context, for the next steps or the reply? (Its content, not just that it ran.)",
            n, tool, preview(arguments.toJson, 200), preview(result.toJson, 600),
          )).json
          val question: Question[?] = mode match
            case ContextChoice.Rate =>
              Question.Score(about, ScoreCriteria(
                "not needed: superseded (the file was read again or edited since), or irrelevant to the request",
                "might help, but nothing ahead depends on it",
                "needed: a later step or the reply uses what it says",
                "essential: the reply or the next edit can't be written without it",
              ).toOption.get)
            case _ =>
              Question.Noul(about, NoulCriteria(
                "a later step or the reply still needs what this result says",
                "it's superseded (the same file read again or edited since, a search refined), or irrelevant to the request",
              ))
          n -> (QuestionId(s"keep_step_$n") -> question)

  /** Whether the decision model's answer about a step keeps it in context; a missing answer keeps it. */
  private def keeps(answer: Option[DynamicAnswer]): Boolean = answer match
    case Some(DynamicAnswer.Noul(p))  => p.unwrap >= 0.5
    case Some(DynamicAnswer.Score(a)) => a.score >= Strategy.KeepRating
    case _                            => true

  private def choice(answers: Map[QuestionId, DynamicAnswer], id: QuestionId): Task[ChoiceAnswer] =
    answers.get(id) match
      case Some(DynamicAnswer.Choice(answer)) => ZIO.succeed(answer)
      case other                              => ZIO.fail(IllegalStateException(s"The decision model gave no choice for ${id.unwrap}: $other"))

  private val NoSecondCall = "No second call: the next action is enough for this step, or a second call would need its result."

  private val AlsoInstructions =
    "Besides next_action, choose a second tool call to run at the same time, only when it doesn't need next_action's " +
      "result and is surely needed too: another search for a different spelling or concept, or reading another file " +
      "the steps have already shown to be relevant. Choose none otherwise. Never the same call as next_action."

  /** The choice's instructions, with what the strategy changes about the options. */
  private def instructions(config: Config, state: State): String =
    val strategy = config.strategy
    List(
      Some(config.instructions),
      Option.when(strategy.catalogInState)("The state's `tools` describes each tool; a call option names the tool it calls."),
      Option.when(strategy.boundCalls)(
        "An option with `arguments` calls its tool with exactly those arguments, taken from an earlier step's result, " +
          "without writing new ones: prefer it when those are the arguments needed.",
      ),
      Option.when(state.filtered)("Only the tools likely relevant to the request are offered; choose more_tools when another is needed."),
      Option.when(state.offered.size > config.groupAbove && state.offered.exists(_.group.isDefined))(
        "Some tools are offered as their group (`open_group`, with its tools' names): choose a group to have its tools " +
          "offered at the next decision when one of them is what the request needs; it costs no step. Prefer a tool " +
          "offered directly when it does the job."),
    ).flatten.mkString("\n\n")

  // ---- the relevance filter ----

  private final case class RelevanceView(request: String, context: Option[String]) derives Schema
  private final case class ToolAbout(question: String, tool: String, description: String, parameters: List[String]) derives Schema

  /** The tools the decision model thinks the request could need (at least [[Strategy.MinRelevant]]), and the decision it took. */
  private def relevance(prompt: String, context: Option[String], tools: Seq[Tool], config: Config): ZIO[DecisionModel, Throwable, (Vector[Tool], Option[LoopTurn])] =
    val ids = tools.zipWithIndex.map((tool, i) => QuestionId(f"tool_$i%03d") -> tool)
    val criteria = NoulCriteria(
      "this tool could be needed for some step of working through the request",
      "nothing the request needs is done with this tool",
    )
    val questions = ids.map: (id, tool) =>
      id -> Question.Noul(
        Content(ToolAbout("Could the agent need this tool for the user's request?", tool.name, preview(tool.description, 600),
          parameters(tool.inputSchema))).json,
        criteria,
      )
    ask(Content(RelevanceView(prompt, context)), questions.toList).map: (answers, usage, latency) =>
      val scored = ids.map: (id, tool) =>
        tool -> (answers.get(id) match
          case Some(DynamicAnswer.Noul(p)) => p.unwrap
          case _                           => 1.0)
      val ranked = scored.sortBy(-_._2)
      val kept = ranked.zipWithIndex.collect { case ((tool, p), i) if p >= Strategy.RelevanceThreshold || i < Strategy.MinRelevant => tool }.toSet
      val probabilities = scored.flatMap((tool, p) => Probability(p).toOption.map(tool.name -> _)).toMap
      val top = ranked.headOption.map(_._1.name).getOrElse("")
      val answer = ChoiceAnswer(top, probabilities, Probability(1.0).toOption.get)
      (tools.toVector.filter(kept), Some(LoopTurn(0, "relevance", answer, usage, latency)))

  /** How many times the arguments are rewritten when they repeat an earlier failed call. */
  private val RewritesOnRepeat = 2

  /** Runs the chosen calls, concurrently when there are several, and records each as a step, in order. */
  /**
   * What's wrong with `arguments` against `schema`, at its top level: required parameters missing, and parameters it
   * doesn't have (unless it allows others); none when they fit.
   */
  def misfit(schema: Json.Obj, arguments: Json.Obj): Option[String] =
    val properties = schema.get("properties").flatMap(_.asObject).map(_.fields.map(_._1).toSet)
    val required = schema.get("required").flatMap(_.asArray).toList.flatten.flatMap(_.asString)
    val open = schema.get("additionalProperties").forall(_ != Json.Bool(false)) && properties.isEmpty
    val missing = required.filterNot(arguments.contains)
    val unknown = properties.fold(Nil)(ps => arguments.fields.map(_._1).filterNot(ps).toList).filter(_ => !open)
    val parts = List(
      Option.when(missing.nonEmpty)(s"missing ${missing.mkString(", ")}"),
      Option.when(unknown.nonEmpty)(s"it has no ${unknown.mkString(", ")}" + properties.fold("")(ps => s" (its parameters: ${ps.toList.sorted.mkString(", ")})")),
    ).flatten
    Option.when(parts.nonEmpty)(parts.mkString("; "))

  /** The steps' outcome note for a proposed call (not a failure of the tool's). */
  private[orchestration] val ProposedNote = "not made"

  /** How many of a loop's proposals are free of its step budget. */
  val FreeProposals = 2

  /**
   * A decision's calls (one, the decision model's second ([[Strategy.parallelCalls]]), or a batch ([[Strategy.batch]])), each a step,
   * numbered in order, at most `parallelism` at once: items of [[Work]], as a plan's parts are.
   */
  private def runCalls(prompt: String, context: Option[String], actions: List[Action], state: State, call: Tools, llm: Llm,
      suppress: Boolean, parallelism: Int = Work.DefaultParallelism): UIO[State] =
    val items = actions.zipWithIndex.map: (action, i) =>
      val number = state.steps.size + 1 + i
      val run: UIO[(Option[Step], Usage, Long, Option[(Tool, Json.Obj)])] = action match
        case Action.Call(tool)            => runCall(prompt, context, tool, None, state, number, call, llm, suppress, state.offered)
        case Action.Bound(tool, args, _)  => runCall(prompt, context, tool, Some(args), state, number, call, llm, suppress, state.offered)
        case _                            => ZIO.succeed((None, Usage(), 0L, None))
      Work.Item[Any, (Option[Step], Usage, Long, Option[(Tool, Json.Obj)])](s"call $number", Work.Kind.Call, run)
    // a call's failure is its step, never the item's: every item ends with its step
    Work.all(items, parallelism).map(_.flatMap(_.last.result.toOption))
    .map: done =>
      // a decision's proposals last until the next one
      done.foldLeft(state.copy(proposed = Vector.empty)):
        case (s, (step, used, toolMs, proposal)) =>
          s.copy(steps = s.steps ++ step, llm = s.llm ++ used, toolLatencyMs = s.toolLatencyMs + toolMs,
            proposed = s.proposed ++ proposal.flatMap((t, a) => step.map(st => (t, a, st.number))).toVector)

  /**
   * Writes the arguments (none for a tool without inputs, the given ones for a bound call), calls the tool, and gives
   * the step with the generative model's usage and the call's time. Arguments that can't be written (the generative
   * model's output didn't parse, say) are a failed step, without a call, so the decision model can try again or choose otherwise; the
   * turn doesn't fail. Arguments that repeat an earlier failed call to the tool are written again (told so), and if
   * they still repeat it, the tool isn't called again: the step fails as a repeat.
   */
  private def runCall(
    prompt: String,
    context: Option[String],
    tool: Tool,
    preset: Option[Json.Obj],
    state: State,
    number: Int,
    call: Tools,
    llm: Llm,
    repeatSuppressed: Boolean,
    offeredTools: Seq[Tool],
  ): UIO[(Option[Step], Usage, Long, Option[(Tool, Json.Obj)])] =
    val needsArguments = preset.isEmpty && tool.inputSchema.get("properties").flatMap(_.asObject).exists(_.fields.nonEmpty)
    def readOnly(name: String): Boolean =
      offeredTools.find(_.name == name).flatMap(_.annotations).flatMap(_.get("readOnlyHint")).contains(Json.Bool(true))
    def failedBefore(arguments: Json.Obj): Option[Step] =
      // a failure another loop's change caused (the work out of date) says nothing about the arguments
      state.steps.find(s => s.tool == tool.name && s.arguments == arguments && (s.outcome match
        case Outcome.Failed(e) => !e.contains(Notices.OutOfDate)
        case _                 => false))
    // a repeat only when nothing since could have changed its result: every successful call after it is read-only
    def ranBefore(arguments: Json.Obj): Option[Step] =
      Option.when(repeatSuppressed)(state.steps.findLast(s => s.tool == tool.name && s.arguments == arguments &&
        s.outcome.isInstanceOf[Outcome.Succeeded])).flatten.filter: earlier =>
          state.steps.drop(earlier.number).forall(s => !s.outcome.isInstanceOf[Outcome.Succeeded] || readOnly(s.tool))

    /** Arguments, the generative model's usage, and the earlier failed step they still repeat, if any. */
    def write(rejected: Vector[Json.Obj], usage: Usage): IO[(Throwable, Usage), (Json.Obj, Usage, Option[Step])] =
      note(if rejected.isEmpty then s"Writing the arguments for ${tool.name}" else s"Writing different arguments for ${tool.name}") *>
      llm.arguments(ArgumentsRequest(prompt, tool, state.visible, state.readings, rejected, context, offeredTools.map(_.name)))
        .mapError(e => (e, usage.copy(calls = usage.calls + 1)))
        .flatMap: written =>
          val used = usage + written
          failedBefore(written.value) match
            case Some(repeat) if rejected.size < RewritesOnRepeat => write(rejected :+ written.value, used)
            case repeat                                           => ZIO.succeed((written.value, used, repeat))

    // arguments that don't fit the tool's schema (a required parameter missing, one it doesn't have) are written again,
    // once, told what's wrong: a call that would only fail costs a tool round trip and a step
    def fitted(written: (Json.Obj, Usage, Option[Step])): IO[(Throwable, Usage), (Json.Obj, Usage, Option[Step])] =
      misfit(tool.inputSchema, written._1) match
        case None => ZIO.succeed(written)
        case Some(problem) =>
          llm.arguments(ArgumentsRequest(prompt, tool, state.visible, state.readings, Vector.empty, context, offeredTools.map(_.name),
              Some(s"These arguments don't fit ${tool.name}'s input schema: $problem. Write them again, against the schema.")))
            .fold(_ => written, again => (again.value, written._2 + again, failedBefore(again.value)))
    val writing: UIO[Either[(Throwable, Usage), (Json.Obj, Usage, Option[Step])]] =
      if needsArguments then write(Vector.empty, Usage()).flatMap(fitted).either
      else
        val arguments = preset.getOrElse(Json.Obj())
        ZIO.succeed(Right((arguments, Usage(), failedBefore(arguments))))
    writing.flatMap:
      // a proposal of a call already made: nothing new to offer; the step says to use its result, and counts
      case Left((Proposed(other, arguments), usage)) if state.steps.exists(s => s.tool == other && s.arguments == arguments) =>
        val earlier = state.steps.findLast(s => s.tool == other && s.arguments == arguments).get
        ZIO.succeed((Some(Step(number, tool.name, Json.Obj(), Outcome.Failed(
          s"not called: the arguments' writer, asked for this call, would repeat step ${earlier.number}'s call of $other " +
            s"instead; its result is in step ${earlier.number}: use it, or choose another tool"))), usage, 0L, None))
      case Left((Proposed(other, arguments), usage)) if offeredTools.exists(_.name == other) =>
        // not a failure to write: the step records what the model wanted, and the next decision offers it ready-made
        val step = Step(number, tool.name, Json.Obj(), Outcome.Failed(
          s"$ProposedNote: the arguments' writer, asked for this call, would call $other instead, with " +
            s"${preview(arguments.toJson, 400)}; that call is offered ready-made"))
        ZIO.succeed((Some(step), usage, 0L, offeredTools.find(_.name == other).map(_ -> arguments)))
      // not a failure to write: what the work must find before this call can be written (it doesn't count against the tool)
      case Left((NeedsMore(_, what), usage)) =>
        ZIO.succeed((Some(Step(number, tool.name, Json.Obj(), Outcome.Failed(
          s"not called: the arguments' writer needs what the work hasn't found yet: ${preview(what, 600)}. Get that first " +
            s"(read the file, look up the definition or an existing use of it), then call ${tool.name} again"))), usage, 0L, None))
      case Left((error, usage)) =>
        val step = Step(number, tool.name, Json.Obj(), Outcome.Failed(s"$Unwritten: ${message(error)}"))
        ZIO.succeed((Some(step), usage, 0L, None))
      case Right((arguments, usage, Some(repeat))) if needsArguments || preset.isDefined =>
        val error = repeat.outcome match
          case Outcome.Failed(e) => e
          case _                 => ""
        val step = Step(number, tool.name, arguments, Outcome.Failed(
          s"not called again: the same arguments already failed in step ${repeat.number} ($error)",
        ))
        ZIO.succeed((Some(step), usage, 0L, None))
      case Right((arguments, usage, _)) if ranBefore(arguments).isDefined =>
        // the same call already succeeded: its result is in the steps, and calling again would only repeat it
        val earlier = ranBefore(arguments).get
        ZIO.succeed((Some(Step(number, tool.name, arguments, Outcome.Failed(
          s"not called again: the same call already ran in step ${earlier.number}; use its result",
        ))), usage, 0L, None))
      case Right((arguments, usage, _)) =>
        // a node of the fiber's process tree, when there's one: it can be seen, and stopped (a failed step) by someone else
        ProcessTree.run(ProcessTree.Kind.Call, tool.name, Some(preview(arguments.toJson, 200)))(call.call(tool.name, arguments))
          .fold(e => Outcome.Failed(message(e)), Outcome.Succeeded(_)).timed.map: (elapsed, outcome) =>
          (Some(Step(number, tool.name, arguments, outcome)), usage, elapsed.toMillis, None)

  /** Reads the document into the state; the documents it links to become readable. */
  private def read(document: Document, state: State): UIO[State] =
    document.read.either.map: content =>
      val reading = Reading(state.readings.size + 1, document.id, document.title, content.map(_.text).left.map(message))
      state.copy(
        readings = state.readings :+ reading,
        documents = distinct(state.documents, content.toOption.toList.flatMap(_.linked)),
      )

  /** `known` followed by those of `more` with an id not seen yet. */
  private def distinct(known: Vector[Document], more: Seq[Document]): Vector[Document] =
    more.foldLeft(known)((all, document) => if all.exists(_.id == document.id) then all else all :+ document)

  private def message(error: Throwable): String = Option(error.getMessage).getOrElse(error.toString)

  // ---- what the decision model sees ----

  private final case class StepView(step: Int, tool: String, arguments: Json, outcome: String, result: Option[String], error: Option[String],
      assessed: Option[String] = None) derives Schema

  private final case class ReadingView(reading: Int, document: String, title: String, text: Option[String], error: Option[String])
      derives Schema

  private final case class ToolView(tool: String, description: String, parameters: List[String], earlierCalls: Int, earlierFailures: Int)
      derives Schema

  private final case class StateView(
    request: String,
    context: Option[String],
    tools: Option[Vector[ToolView]],
    steps: Vector[StepView],
    documentsRead: Vector[ReadingView],
    stepsRemaining: Int,
    readingsRemaining: Int,
  ) derives Schema

  private def stateView(prompt: String, context: Option[String], tools: Seq[Tool], state: State, config: Config): Content =
    Content(StateView(
      prompt,
      context,
      Option.when(config.strategy.catalogInState)(state.offered.map: tool =>
        val earlier = state.steps.filter(_.tool == tool.name)
        ToolView(tool.name, tool.description, parameters(tool.inputSchema), earlier.size, earlier.count(_.outcome.isInstanceOf[Outcome.Failed]))
      ),
      budgeted(state.steps, config, state.dropped).map(v => v.copy(assessed = state.assessed.get(v.step).map(_.describe))),
      state.readings.map(r => ReadingView(r.number, r.document, r.title, r.text.toOption.map(preview(_, config.resultPreview)), r.text.left.toOption)),
      (config.maxSteps - state.stepsTaken).max(0),
      (config.maxReadings - state.readings.size).max(0),
    ))

  /** How much of an older step's result the decision model sees once the state budget is spent. */
  val OlderPreview = 300

  /** The steps as the decision model sees them: the newest with their results up to `resultPreview`, older ones shorter once the budget is spent. */
  private def budgeted(steps: Vector[Step], config: Config, dropped: Set[Int] = Set.empty): Vector[StepView] =
    steps.reverse.foldLeft((config.stateBudget, Vector.empty[StepView])):
      case ((left, out), step) if dropped(step.number) =>
        val view = this.view(step, OlderPreview)
        (left, view.copy(outcome = "succeeded; result left out of context as no longer needed", result = None) +: out)
      case ((left, out), step) =>
        val max = if left > 0 then config.resultPreview.min(left.max(OlderPreview)) else OlderPreview
        val view = this.view(step, max)
        (left - view.result.orElse(view.error).fold(0)(_.length) - view.arguments.toJson.length, view +: out)
    ._2

  private def view(step: Step, max: Int): StepView =
    // arguments too (a patch, a file's text) are cut for the decision model; the step's full arguments stay in the loop's state
    val arguments = if step.arguments.toJson.length <= max then step.arguments else Json.Str(preview(step.arguments.toJson, max))
    step.outcome match
      case Outcome.Succeeded(result) =>
        StepView(step.number, step.tool, arguments, "succeeded", Some(preview(result.toJson, max)), None)
      case Outcome.Failed(error) =>
        StepView(step.number, step.tool, arguments, "failed", None, Some(preview(error, max)))

  private def preview(text: String, max: Int): String =
    if text.length <= max then text else text.take(max) + s"… (${text.length - max} more characters)"

  private final case class ToolOption(
    action: String,
    tool: String,
    description: String,
    parameters: Option[List[String]],
    inputSchema: Option[Json],
    outputSchema: Option[Json],
    annotations: Option[Json],
    earlierCalls: Int,
    earlierFailures: Int,
  ) derives Schema

  private final case class ReadOption(action: String, document: String, title: String, description: String, metadata: Map[String, String])
      derives Schema

  private final case class EndOption(action: String, outcome: String, description: String) derives Schema

  private val answer = EndOption(
    "end",
    "answer",
    "End the loop and answer the request from what the steps found, or directly when it needs no tool.",
  )

  private val unable = EndOption(
    "end",
    "unable",
    "End the loop and report that the request can't be completed with the available tools, and why.",
  )

  /**
   * A tool's parameters, short: each name with its type (an enum's values instead), required ones marked
   * (`file_path: string (required)`, `analysisKind: INCOMING_CALLS|OUTGOING_CALLS (required)`).
   */
  private[orchestration] def parameters(inputSchema: Json.Obj): List[String] =
    val required = inputSchema.get("required").flatMap(_.asArray).toList.flatten.flatMap(_.asString).toSet
    inputSchema.get("properties").flatMap(_.asObject).toList.flatMap(_.fields).map: (name, schema) =>
      val obj = schema.asObject
      val kind = obj.flatMap(_.get("enum")).flatMap(_.asArray).map(_.flatMap(_.asString).mkString("|"))
        .orElse(obj.flatMap(_.get("type")).flatMap(_.asString)).getOrElse("any")
      s"$name: $kind${if required(name) then " (required)" else ""}"

  /** One option of the decision model's choice. */
  private final case class Opt(id: String, action: Action, description: Content)

  private final case class NamedToolOption(action: String, tool: String, earlierCalls: Int, earlierFailures: Int) derives Schema
  private final case class BoundOption(action: String, tool: String, arguments: Json, from: String) derives Schema

  /** The longest string value bound from an earlier result into a ready-made call ([[bindings]]). */
  val MaxBoundValue = 300

  /**
   * A ready-made call's arguments as an option shows them: each string value cut to `cap` characters (a proposed
   * edit's text, a rerun's long argument), the call itself made with them whole.
   */
  private def cutArguments(arguments: Json.Obj, cap: Int): Json =
    def cut(j: Json): Json = j match
      case Json.Str(v) if v.length > cap => Json.Str(v.take(cap) + s"… (${v.length - cap} more characters)")
      case Json.Obj(fs)                  => Json.Obj(fs.map((k, v) => k -> cut(v)))
      case Json.Arr(xs)                  => Json.Arr(xs.map(cut))
      case other                         => other
    cut(arguments)
  private final case class MoreToolsOption(action: String, description: String) derives Schema
  private final case class GroupOption(action: String, group: String, description: String, tools: List[String]) derives Schema

  /** How many tool groups there are: each can be opened once, a decision that makes no call. */
  private def groupCount(tools: Seq[Tool]): Int = tools.flatMap(_.group).distinct.size

  /** Whether the loop's last step was a successful call of a tool that is the request's whole work ([[Config.endsTurn]]). */
  private def endedByCall(state: State, tools: Seq[Tool], config: Config): Boolean =
    state.steps.lastOption.exists(s => s.outcome.isInstanceOf[Outcome.Succeeded] && tools.exists(t => t.name == s.tool && config.endsTurn(t)))

  private def options(tools: Seq[Tool], state: State, config: Config, descriptionCap: Int = Int.MaxValue): Vector[Opt] =
    val endings = Vector(
      Opt("end_answer", Action.End(Ending.Answered), Content(answer)),
      Opt("end_unable", Action.End(Ending.Unable), Content(unable)),
    )
    // a loop the decision model judged stuck (its checks keep failing the same way) is offered only the endings
    val stuck = config.strategy.giveUp && state.steps.lastOption.exists(s => state.assessed.get(s.number).exists(_.stuck))
    // a call that, done, is the request's whole work (it started something that goes on by itself) ends the loop
    val finished = endedByCall(state, tools, config)
    if state.stepsTaken + state.running.size >= config.maxSteps || stuck || finished || state.halted then endings ++ Option.when(state.running.nonEmpty)(
      Opt("wait", Action.Await, Content(BackgroundOption("wait", "", "Wait for a background call to end before ending.")))).toVector
    else
      // a tool whose arguments couldn't be written twice isn't offered again this turn: the model can't write them
      val unwritable = state.steps.collect { case Step(_, tool, _, Outcome.Failed(e)) if e.startsWith(Unwritten) => tool }
        .groupBy(identity).collect { case (tool, n) if n.size >= 2 => tool }.toSet
      // a loop told to act isn't offered its reading tools
      val act = acting(state, tools, config)
      // a large catalog's grouped tools are offered as their groups, until opened (a tool once called stays offered)
      val tiered = state.offered.size > config.groupAbove
      val used = state.steps.map(_.tool).toSet
      def direct(t: Tool) = !tiered || t.group.forall(g => state.opened(g.name)) || used(t.name)
      val groups = Option.when(tiered)(state.offered.filterNot(direct).filterNot(t => unwritable(t.name) || (act && config.explores(t)))
        .groupBy(_.group).collect { case (Some(g), ts) => g -> ts }.toVector.sortBy(_._1.name)).toVector.flatten
        .zipWithIndex.map: (gt, i) =>
          val (g, ts) = gt
          Opt(f"group_$i%03d", Action.OpenGroup(g), Content(GroupOption("open_group", g.name, g.description, ts.map(_.name).toList)))
      val calls = state.offered.filter(direct).filterNot(t => unwritable(t.name) || (act && config.explores(t))).map: tool =>
        val index = tools.indexWhere(_.name == tool.name)
        val earlier = state.steps.filter(_.tool == tool.name)
        val description =
          if config.strategy.catalogInState then
            Content(NamedToolOption("call_tool", tool.name, earlier.size, earlier.count(_.outcome.isInstanceOf[Outcome.Failed])))
          else Content(ToolOption(
            "call_tool",
            tool.name,
            cutDescription(tool.description, descriptionCap),
            Option.unless(tool.schemasInChoice)(parameters(tool.inputSchema)),
            Option.when(tool.schemasInChoice)(tool.inputSchema),
            tool.outputSchema.filter(_ => tool.schemasInChoice),
            tool.annotations,
            earlier.size,
            earlier.count(_.outcome.isInstanceOf[Outcome.Failed]),
          ))
        Opt(f"tool_$index%03d", Action.Call(tool), description)
      // a check made before, made again exactly (a test run after an edit): ready-made, no generative model
      val again =
        if !config.strategy.boundCalls then Vector.empty
        else rerunnable(state, config).zipWithIndex.map: (r, i) =>
          val (tool, arguments, step) = r
          Opt(f"again_$i%03d", Action.Bound(tool, arguments, step),
            Content(BoundOption("call_tool_with", tool.name, cutArguments(arguments, descriptionCap.min(600)), s"the same check as step $step, run again (the code has changed since)")))
      // what the arguments' writer proposed at the last step, ready-made; then the bindings from earlier results
      val proposed = state.proposed.zipWithIndex.map: (p, i) =>
        val (tool, arguments, step) = p
        Opt(f"proposed_$i%03d", Action.Bound(tool, arguments, step),
          Content(BoundOption("call_tool_with", tool.name, cutArguments(arguments, descriptionCap.min(600)), s"the call the arguments' writer proposed at step $step")))
      val bound = (proposed ++ again ++ (
        if !config.strategy.boundCalls then Vector.empty
        else bindings(state).zipWithIndex.map: (b, i) =>
          Opt(f"bound_$i%03d", b, b match
            case Action.Bound(tool, arguments, source) =>
              Content(BoundOption("call_tool_with", tool.name, cutArguments(arguments, descriptionCap.min(600)), s"values from step $source's result"))
            case _ => Content(""))))
        // nor ready-made reads, nor documents to read
        .filterNot(o => act && (o.action match
          case Action.Bound(tool, _, _) => config.explores(tool)
          case _                        => false))
      val more =
        if state.filtered then Vector(Opt("more_tools", Action.MoreTools, Content(MoreToolsOption("more_tools",
          s"Offer the ${tools.size - state.offered.size} tools left out as unlikely to be relevant: ${tools.filterNot(state.offered.contains).map(_.name).mkString(", ")}"))))
        else Vector.empty
      val room = Decisions.ChoiceCriteria.MaxOptions - endings.size - calls.size - groups.size - bound.size - more.size
      val reads =
        if state.readings.size >= config.maxReadings || act then Vector.empty
        else state.unread.take(room.max(0)).zipWithIndex.map: (document, index) =>
          Opt(f"doc_$index%03d", Action.Read(document),
            Content(ReadOption("read_document", document.id, document.title, document.description, document.metadata)))
      val inBackground =
        if !config.strategy.background then Vector.empty
        else state.offered.filter(config.isCheck).filterNot(t => state.running.exists(_.tool.name == t.name)).map: tool =>
          Opt(f"bg_${tools.indexWhere(_.name == tool.name)}%03d", Action.Background(tool), Content(BackgroundOption("call_tool_in_background",
            tool.name, "Start this call and go on without waiting (read, edit, prepare the next change); its result comes as a " +
              "step at the first decision after it ends.")))
      val waiting = Option.when(state.running.nonEmpty)(Opt("wait", Action.Await, Content(BackgroundOption("wait", "",
        "Wait for a background call to end: the next decision comes as soon as one does. Choose it when nothing else is " +
          "worth doing until its result is known.")))).toVector
      val workflow = Option.when(config.strategy.workflows && config.maxSteps - state.stepsTaken >= 2)(
        Opt("workflow", Action.Workflow, Content(WorkflowOption("workflow", WorkflowDescription)))).toVector
      // a batch: independent calls at once, when there's a tool to make them with
      val batch = Option.when(config.strategy.batch && batchable(tools, state, config).nonEmpty)(
        Opt("batch", Action.Batch, Content(WorkflowOption("batch", BatchDescription)))).toVector
      // a loop with a goal (its node's in the process tree) can revise it, a few times
      val revise = Option.when(state.goal.isDefined && state.revisions < MaxRevisions && state.steps.nonEmpty)(
        Opt("revise_goal", Action.ReviseGoal, Content(WorkflowOption("revise_goal", ReviseGoalDescription)))).toVector
      calls ++ groups ++ bound.take((Decisions.ChoiceCriteria.MaxOptions - endings.size - calls.size - groups.size - more.size - 5 -
        inBackground.size - waiting.size - revise.size).max(0)) ++ inBackground ++ waiting ++ workflow ++ batch ++ more ++ reads ++ moves(state, config) ++
        revise ++ endings

  /**
   * Ready-made calls: for each tool whose required parameters (one, a string) an earlier successful result holds a
   * value for, under the same name (case, `_` and `-` aside: a result's `filePath` is a tool's `file_path`), a call
   * with that value; none repeating an earlier call, at most [[Strategy.MaxBound]]: the tools already called first,
   * then the ones the decision model thought likeliest at its last decision, newer results first.
   */
  /**
   * The checks made before (test runs, builds) that something changed since (a step that isn't a read or a check came
   * after), each once with its latest arguments, newest first, at most 3: run again, they need no arguments written.
   */
  private def rerunnable(state: State, config: Config): Vector[(Tool, Json.Obj, Int)] =
    def readOnly(t: Tool) = t.annotations.exists(_.get("readOnlyHint").contains(Json.Bool(true)))
    val checks = state.steps.flatMap(s => state.offered.find(_.name == s.tool).filter(config.isCheck).map(t => (t, s.arguments, s.number)))
    checks.reverse.distinctBy((t, a, _) => (t.name, a.toJson)).filter: (_, a, n) =>
      state.steps.drop(n).exists(s => state.offered.find(_.name == s.tool).exists(t => !config.isCheck(t) && !readOnly(t)) &&
        s.outcome.isInstanceOf[Outcome.Succeeded])
    .filterNot((t, a, _) => state.steps.lastOption.exists(l => l.tool == t.name && l.arguments == a))
    .take(3)

  private def bindings(state: State): Vector[Action] =
    def norm(name: String) = name.toLowerCase.filter(_.isLetterOrDigit)
    def strings(json: Json, key: Option[String]): Vector[(String, String)] = json match
      case Json.Obj(fields) => fields.toVector.flatMap((k, v) => strings(v, Some(k)))
      case Json.Arr(items)  => items.toVector.flatMap(strings(_, key))
      // a value worth binding names something (a path, an id, a symbol): not a log or a file's text
      case Json.Str(value)  => key.toVector.map(k => norm(k) -> value).filter((_, v) => v.nonEmpty && v.length <= MaxBoundValue && !v.contains('\n'))
      case _                => Vector.empty
    // results may be JSON text in a `text` field (an MCP tool's text content)
    def values(result: Json): Vector[(String, String)] =
      val parsed = result.asObject.flatMap(_.get("text")).flatMap(_.asString).flatMap(_.fromJson[Json].toOption).toVector
      (result +: parsed).flatMap(strings(_, None))
    val tools = state.offered.flatMap: tool =>
      val required = tool.inputSchema.get("required").flatMap(_.asArray).toList.flatten.flatMap(_.asString)
      val props = tool.inputSchema.get("properties").flatMap(_.asObject)
      required match
        case name :: Nil if props.flatMap(_.get(name)).flatMap(_.asObject).flatMap(_.get("type")).contains(Json.Str("string")) =>
          Some((tool, name))
        case _ => None
    val found = state.steps.reverse.collect { case Step(n, _, _, Outcome.Succeeded(result)) => n -> values(result) }
    val calls = for
      (source, vals)  <- found
      (tool, param)   <- tools
      value           <- vals.collect { case (k, v) if k == norm(param) => v }.distinct
      arguments        = Json.Obj(param -> Json.Str(value))
      if !state.steps.exists(s => s.tool == tool.name && s.arguments == arguments)
    yield (tool, arguments, source)
    // the tools the decision model has called, then those it thought likeliest at its last decision
    val used = state.steps.map(_.tool).toSet
    def rank(tool: Tool) = (if used(tool.name) then 0 else 1, -state.lastChoice.getOrElse(tool.name, 0.0))
    calls.distinctBy((tool, arguments, _) => (tool.name, arguments.toJson)).sortBy((tool, _, source) => (rank(tool), -source))
      .take(Strategy.MaxBound)
      .map((tool, arguments, source) => Action.Bound(tool, arguments, source))
