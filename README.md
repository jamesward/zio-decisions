# zio-decisions

[![javadocs.dev](https://www.javadocs.dev/com.jamesward/zio-decisions_3/badge.svg)](https://www.javadocs.dev/com.jamesward/zio-decisions_3/latest)

A Scala 3 / ZIO library for decision models: "System One" models that answer
typed, atomic questions about a piece of state with calibrated probabilities,
instead of generating text. One provider-neutral API, with providers for
[TypeSafe AI's Jev](https://docs.typesafe.ai/introduction) and
[OpenAI's Decisions API](https://developers.openai.com/api/docs/guides/decisions).

- Provider-neutral. `ask`, `loop`, and the whole `orchestration` package are
  written against one trait, `DecisionModel`; switching providers is
  switching one layer.
- Typed end-to-end. No raw JSON, no stringly-typed answer lookups.
- Ask several questions in one round-trip via a `NamedTuple`; the typed
  answer comes back keyed and shaped exactly like the questions you asked.
- Illegal states are unrepresentable: `ChoiceCriteria` / `ScoreCriteria`
  can't be built out of range (1–255 options, 2–10 levels), and every
  probability / confidence is a `Probability` — a `Double` provably within
  `[0.0, 1.0]`.
- Built on ZIO HTTP's `Client`.

## Install

```scala
libraryDependencies += "com.jamesward" %% "zio-decisions" % "<version>"
```

## Providers

| Provider | Layer | Reads | Default model |
| --- | --- | --- | --- |
| TypeSafe AI (Jev / System One) | `typesafe.TypeSafe.live` | `TYPESAFE_API_KEY`, `TYPESAFE_DEFAULT_MODEL` | `jev-latest` |
| OpenAI Decisions API | `openai.OpenAI.live` | `OPENAI_API_KEY`, `OPENAI_DECISIONS_MODEL` | `gpt-6-luna` |

Each provider is a `ZLayer[Client, Error, DecisionModel]`; provide one and
everything else stays the same:

```scala
import com.jamesward.zio_decisions.typesafe.TypeSafe
import com.jamesward.zio_decisions.openai.OpenAI
import zio.http.Client

program.provide(Client.default, TypeSafe.live)
program.provide(Client.default, OpenAI.live)
```

Or construct a layer explicitly (optionally against another origin, such as a
proxy or a test double):

```scala
TypeSafe.layer(TypeSafe.ApiKey("…"), TypeSafe.JevLatest)
OpenAI.layer(OpenAI.ApiKey("sk-…"), OpenAI.DefaultModel)
```

How the questions map onto each wire:

| Question | TypeSafe | OpenAI |
| --- | --- | --- |
| `Noul` | `noul`, with optional `true` / `false` criteria | `predicate`; criteria appended to the instructions |
| `Choice` | `choice`, criteria `{option: description}` | `choice`, `[{value, description}]` |
| `Score` | `score`, criteria `[level, …]` | `score`, `[{label: "Level i", description}]` |

TypeSafe takes structured (object/array) state, instructions and descriptions
as JSON; OpenAI's `input` and instructions are text, so a non-string value is
sent as its JSON text. OpenAI may refuse a question: that fails the request
with `Error.Refused(questionId)`.

To bring another model, implement the trait:

```scala
trait DecisionModel:
  def provider: String
  def defaultModel: ModelId
  def decide(request: DecisionModel.Request): IO[Error, DecisionModel.Decision]
```

### Middleware

Decorate any decision model layer with composable middleware using the same
`@@` / `++` shape as ZIO HTTP. Composition applies the left middleware around
the right:

```scala
import com.jamesward.zio_decisions.{DecisionModel, Middleware}

val model    = OpenAI.live @@ Middleware.logging()
val composed = TypeSafe.live @@ (metricsMiddleware ++ Middleware.logging())
```

`Middleware.make` wraps the exchange effect to add retries, tracing, metrics,
policy checks, or other cross-cutting behavior. It receives the request (typed,
and as provider-neutral canonical JSON in `request.body`) and an effect
producing the decision:

```scala
val retryRateLimits = Middleware.make: (_, next) =>
  next.catchSome:
    case _: Error.RateLimit => next
```

`Middleware.logging()` logs complete requests, canonical responses, failures,
and latency. Configure request and response body inclusion with
`Middleware.LoggingConfig`. Headers and bearer tokens are never included, but
state and model output may still be sensitive, so logging is opt-in.

`ExchangeObserver` and `DecisionModel.observed` observe every physical exchange
and are implemented through middleware. Observed bodies are the canonical
JSON, the same whichever provider answered. Callback defects and
self-interruption are logged and suppressed without changing the exchange
result.

## Asking questions

Decision models have three question primitives:

- **`Question.Noul`**   — "is this true?" → a calibrated [`Probability`].
- **`Question.Choice`** — "which of these?" → the selected option plus the
  full probability distribution.
- **`Question.Score`**  — "which level?" → a probability-weighted mean over
  an ordered scale.

Bundle one or more questions in a `NamedTuple` passed to `Decisions.ask`.
The keys become both the wire question ids *and* the compile-time field
names of the typed result — ask for `isUrgent` and `department`, get back
a value with `.isUrgent: Probability` and `.department: ChoiceAnswer`.

```scala
import com.jamesward.zio_decisions.Decisions.*
import com.jamesward.zio_decisions.openai.OpenAI

val message =
  "Hi, I've been trying to connect my Stripe account for 3 days and it " +
  "keeps failing. I'm losing sales. Please help ASAP."

val department: ChoiceCriteria =
  ChoiceCriteria(
    "billing"   -> "Payment or subscription issues",
    "technical" -> "Bugs or integration problems",
    "sales"     -> "Pricing or account questions",
  ).getOrElse(throw new IllegalArgumentException("bad criteria"))

val frustration: ScoreCriteria =
  ScoreCriteria(
    "Calm, just stating facts",
    "Frustrated but civil",
    "Very angry, strong language",
  ).getOrElse(throw new IllegalArgumentException("bad criteria"))

val program = ask(
  message,
  (
    isUrgent    = Question.Noul("Does this message express urgency or time pressure?"),
    department  = Question.Choice("Which team should handle this?", department),
    frustration = Question.Score("How frustrated does the customer sound?", frustration),
  ),
).answers
// : ZIO[DecisionModel, Error, (isUrgent: Probability, department: ChoiceAnswer, frustration: ScoreAnswer)]

program.debug("answers").provide(zio.http.Client.default, OpenAI.live)
```

### Advanced structured primitives

Every TypeSafe [`EntryType`](https://docs.typesafe.ai/primitives/advanced) shape is preserved on TypeSafe's wire (OpenAI receives structured values as their JSON text):

| Field | Scala values |
| --- | --- |
| Choice/Score/Noul `instructions` | Any `Schema` value: `String`, a case class or `Map` (object), a collection (array), or `Option.empty[A]` (JSON `null`) |
| Choice option descriptions | `Content` containing any structured value, or `null` |
| Score level descriptions | `Content` containing any structured value, or `null` |
| Noul `true` / `false` descriptions | `Content` containing any structured value, or `null`; omit the entire criteria object by not passing it to `Question.Noul` |

This means schemas, taxonomies, database rows, rubrics, and boundary examples can be passed as data rather than interpolated into prompts:

```scala
import zio.schema.{Schema, derived}

case class FieldCheck(
  question: String,
  compare: List[String],
  focus: String,
) derives Schema

case class Boundary(definition: String, examples: List[String]) derives Schema

val routeCriteria = ChoiceCriteria.fromContent(Map(
  "billing" -> Content(Boundary("A payment or subscription issue", List("card declined"))),
  "technical" -> Content(Map(
    "includes" -> List("bugs", "integration failures"),
    "excludes" -> List("pricing questions"),
  )),
  "other" -> (null: Content | Null),
)).toOption.get

val severityCriteria = ScoreCriteria.fromContent(List(
  Content(Boundary("No customer impact", List("cosmetic typo"))),
  Content(List("degraded", "workaround available")),
  null,
)).toOption.get

val identityBoundary = NoulCriteria.fromContent(
  whenTrue = Content(Boundary("The identity conflicts with the domain", List("Acme via an unrelated domain"))),
  whenFalse = null,
)

val questions = (
  route = Question.Choice(
    FieldCheck(
      "Which team owns this issue?",
      List("ticket.subject", "ticket.body"),
      "Use the most specific matching rubric.",
    ),
    routeCriteria,
  ),
  severity = Question.Score(List("Assess customer impact", "Prefer observed evidence"), severityCriteria),
  identityConflict = Question.Noul(
    Map("compare" -> List("ticket.sender.display_name", "ticket.sender.email")),
    identityBoundary,
  ),
)
```

The same API also accepts dynamic `zio.json.ast.Json` values. Import ZIO Schema's JSON-AST schema when the state or instructions are assembled at runtime; use `Content` for dynamic criteria entries:

```scala
import zio.json.ast.Json
import zio.schema.codec.json.schemaJson

val dynamicInstructions: Json = Json.Obj(
  "question" -> Json.Str("Which route applies?"),
  "compare" -> Json.Arr(Json.Str("subject"), Json.Str("body")),
)

val dynamicCriteria = ChoiceCriteria.fromContent(Map(
  "billing" -> Content(Json.Obj("examples" -> Json.Arr(Json.Str("card declined")))),
  "other" -> (null: Content | Null),
)).toOption.get

val dynamicQuestion = Question.Choice(dynamicInstructions, dynamicCriteria)
val dynamicState: Json = Json.Obj("subject" -> Json.Str("Payment failed"))
val dynamicRequest = ask(dynamicState, (route = dynamicQuestion))
```

Both paths use the same `Schema[T]` encoding boundary: domain values retain their concrete type in the Scala API, while `Json` supports runtime-defined object, array, string, and null shapes without defining a case class.

For a taxonomy walk, build each `ChoiceCriteria` from the current node's children and put each child subtree in its `Content` description. Use the returned probability distribution to decide whether to continue one branch or retain several candidates.

`ask(state, questions)` requires a `DecisionModel` in the environment and a
non-empty `NamedTuple` of `Question`s; passing anything else is a compile
error. `state` and every question's `instructions` are generic in a
`Schema` type parameter — `ask[S: Schema](state: S, ...)`,
`Question.Noul[S: Schema](instructions: S, ...)` — so a plain string
works out of the box, and any other `Schema`-derived record, taxonomy,
or database row works exactly the same way, with `S` inferred from
whatever you pass. `question.instructions` reads back as that exact
value, precisely typed — no wrapper, no decode step.

### `.answers` vs `.run`

`.answers` returns just the typed `NamedTuple`. `.run` wraps it in a
`Result`, which also carries the responding model and token usage:

```scala
ask(message, (isUrgent = Question.Noul("Urgent?"))).run
// : ZIO[DecisionModel, Error, Result[(isUrgent: Probability)]]
//   Result(answers, model, usage)
```

### Overriding the model

```scala
ask(message, tools).model(ModelId("jev-2025-11")).answers
```

## Criteria smart constructors

`ChoiceCriteria` and `ScoreCriteria` validate their shape at
construction — the model accepts 1–255 choice options and 2–10 score levels,
so building one outside that range is a compile-valid but always-`Left`
call, not a runtime exception surfacing later on the wire:

```scala
ChoiceCriteria("yes" -> "...", "no" -> "...")           // Either[String, ChoiceCriteria]
ChoiceCriteria.fromContent(structuredOptionsMap)         // for object/array descriptions

ScoreCriteria("low", "medium", "high")                   // Either[String, ScoreCriteria]
ScoreCriteria.fromContent(structuredLevelsList)          // for object/array levels
```

A `null` `Choice` option description means "the option name is
self-explanatory" (matches the wire's own `null` convention).

## Decision-model-driven state-machine loops

`Decisions.loop` repeatedly presents a host-generated finite action set to
the decision model as one `Choice`, handles the selected action, and either continues with a
new state or completes. State and action types remain application-defined.
The wire request contains the serialized `Content` state view, option ids and
descriptions, configurable choice instructions, model id, and question
metadata; the host action values themselves never cross the wire.

```scala
val request = Decisions.loop[Int, String, Any, Nothing, Int](0)(
  state => Content(state),
  _ => ZIO.succeed(NonEmptyChunk(
    LoopOption.text("increment", "increment", "Increase the state by one."),
    LoopOption.text("finish", "finish", "Return the current state."),
  )),
) { (state, action) =>
  action match
    case "increment" => ZIO.succeed(LoopStep.Continue(state + 1))
    case "finish"    => ZIO.succeed(LoopStep.Done(state))
}

request.maxIterations(10).run
// ZIO[DecisionModel, Error, LoopResult[Int]]
```

`LoopResult` contains the final output, every `LoopTurn` (selected choice,
full `ChoiceAnswer`, usage, and decision request latency), aggregate decision usage, and
aggregate decision request latency. Option ids must be non-empty and unique. Unknown
model choices, invalid options/policies, and non-positive iteration limits fail
with `Error.InvalidLoop`; exhausting the limit fails with
`Error.MaxIterations`.

The default Choice instruction sentence is stable and unchanged. Replace it
with any `Schema` value—objects and arrays remain structured on the wire—and
configure retry/observation independently:

```scala
val observer = LoopObserver.make[MyError, Output] {
  case LoopObservation.OptionsGenerated(iteration, state, options) => recordOptions(iteration, state, options)
  case LoopObservation.Decision(turn)                              => recordDecision(turn)
  case LoopObservation.Completion(result)                          => recordCompletion(result)
  case LoopObservation.Failure(cause, turns)                       => recordFailure(cause, turns)
}

request
  .choiceInstructions(Map("goal" -> "finish", "rule" -> "pick one legal option"))
  .retryEachTurn(LoopRetryPolicy(maxRetries = 2, initialDelay = Duration.Zero))
  .observe(observer)
  .run
```

Semantic event order is validated options → `OptionsGenerated` → the decision →
`Decision` → the handler → recursion or `Completion`. A failed execution emits
exactly one terminal `Failure` with all successfully selected turns; its cause
is `Cause[Error | E]`, so host error types do not need to extend `Throwable`.
Callbacks are best-effort and defect/interruption-isolated.

Retries are opt-in and apply only to the current turn's decision request. Only
`RateLimit`, `ServiceOverloaded`, and `InternalServer` are retryable; option
generation, state rendering, observers, handlers, and recursion are never
replayed. One successful decision still produces one handler call and one
`LoopTurn`. Turn latency includes all physical attempts and backoff waits;
usage contains the successful response's reported tokens because failed
responses provide no usage.

Call `.runAudited` to additionally retain one `LoopAuditTurn` per successful
decision: iteration, `Content` state view, and option id/description pairs.
`AuditedLoopResult` delegates `output`, `turns`, `usage`, and `latencyMs` to its
unchanged `LoopResult`. Audit is opt-in because state/option snapshots consume
memory and may contain sensitive application data; ordinary `.run` returns the
same `LoopResult` shape as before.

An action handler may run arbitrary ZIO effects—MCP calls, database operations,
or a no-tool generative model call—while the model remains the outer decision loop.

See [docs/loop-architecture.md](docs/loop-architecture.md) for diagrams of the
loop's components, one iteration's request path, its per-iteration validation
gates and failure exits, and what `LoopResult` accumulates.

## Errors

One error type for every provider (`Decisions.Error`); HTTP providers map
their status codes onto the same cases:

```scala
sealed trait Error extends Throwable
object Error:
  final case class BadRequest         (message: String)                 extends Error  // 400
  final case class Authentication     (message: String)                 extends Error  // 401
  final case class PermissionDenied   (message: String)                 extends Error  // 403
  final case class NotFound           (message: String)                 extends Error  // 404
  final case class UnprocessableEntity(message: String)                 extends Error  // 422
  final case class RateLimit          (message: String)                 extends Error  // 429
  final case class ServiceOverloaded  (message: String)                 extends Error  // 529
  final case class InternalServer     (status: Status, message: String) extends Error  // 5xx
  final case class Unexpected         (status: Status, body: String)    extends Error
  final case class Transport          (cause: Throwable)                extends Error
  final case class MissingApiKey      (variable: String)                extends Error
  final case class TooLong            (message: String)                 extends Error  // over the model's context
  // Protocol / decode
  final case class MalformedAnswer(questionId: QuestionId, message: String) extends Error
  final case class MissingAnswer  (questionId: QuestionId)                  extends Error
  final case class Refused        (questionId: QuestionId)                  extends Error  // OpenAI's refusal

  def isTransient(error: Error): Boolean   // Transport / RateLimit / ServiceOverloaded / InternalServer

  extension [R, A](zio: ZIO[R, Error, A])
    /** Retries RateLimit / ServiceOverloaded / InternalServer up to 2x with backoff. */
    def retryOnRetryable: ZIO[R, Error, A]
```

`MalformedAnswer` / `MissingAnswer` only fire on an actual protocol
violation (an out-of-range probability, an answer for the wrong question
type, a question the model never answered) — never on ordinary model output.

## File layout

```
src/main/scala/com/jamesward/zio_decisions/
  Decisions.scala            — questions, criteria, answers, Content, Error, ask / askDynamic / loop
  DecisionModel.scala        — the provider trait, Request / Decision, observed, @@
  Middleware.scala           — composable middleware around every exchange
  internal/                  — Codecs, Canonical (the neutral JSON), RequestImpl, LoopImpl
  typesafe/                  — TypeSafe (Jev / System One) provider + its Wire types
  openai/                    — OpenAI (Decisions API) provider + its Wire types
  orchestration/             — agent loops and orchestration on any DecisionModel

src/test/scala/com/jamesward/zio_decisions/
  TypeSafeMock.scala         — scripted Jev (a TypeSafe model over a scripted wire)
  SharedSpec.scala           — scenarios shared by the mock and every live provider
  TypeSafeMockSpec.scala, TypeSafeIntegrationSpec.scala
  OpenAISpec.scala           — against a fake /v1/decisions
  OpenAIIntegrationSpec.scala
```

The live integration specs run when `TYPESAFE_API_KEY` / `OPENAI_API_KEY` are
set, and are skipped otherwise. Locally, put them in a gitignored `.env`:
sbt-dotenv passes it to the forked test JVM.

The `internal` package is never imported by users. `zio.schema.DynamicValue`
doesn't appear anywhere in this codebase. In the typed `ask` API, `state` and
every question's `instructions` stay precisely typed as `S`; heterogeneous
criteria descriptions use `Content` as their JSON erasure boundary. The loop
API also uses `Content` deliberately for serialized state views, option
and choice-instruction descriptions, semantic observations, and audit
snapshots.

When a transition needs the decision model's calibrated distribution immediately, use `loopWithTurn`. Its handler receives the completed `LoopTurn` before recursion:

```scala
val probabilityAware = Decisions.loopWithTurn(initial)(stateView, options) {
  (state, selectedAction, turn) =>
    val selectedProbability = turn.answer.probabilities(turn.choice)
    updateBeam(state, selectedAction, selectedProbability, turn.answer.confidence)
}
```

`loop` remains source-compatible and is equivalent to `loopWithTurn` with the third handler argument ignored. `loopWithTurn` is useful for confidence gates, probability-aware transitions, and hierarchical beam traversal.

## Agent loop: a decision model as an agent's orchestrator

`orchestration.AgentLoop` is an agentic loop on any `DecisionModel`, with the decision model choosing every step. Each step is
one decision-model choice among one option per tool (its name, description, input and output schemas, annotations, and how many
earlier calls to it there were and how many failed) and two endings: end with an answer (from what the steps found,
or directly when the request needs no tool) and end as unable (no tool can get what's needed, or the needed calls keep
failing). The decision model sees the request and every step taken, each tool result cut to `Config.resultPreview` characters. A
tool with `schemasInChoice = false` is offered by name, description, annotations and its parameters in short (name,
type or enum values, whether required), for toolsets whose schemas together are too large for one choice; its
arguments are still written against its input schema.

The decision model only chooses. A generative model, through the `AgentLoop.Llm` you supply, writes a chosen tool's arguments
(structured output against its input schema; a tool without inputs gets none) and the reply, as an answer, an
explanation of what couldn't be done, or, when `Config.maxSteps` tool calls have been made and only the endings are
offered, an answer that says what's missing. It is never given tools. A failed call is a step like any other, so the decision model
can try another tool or give up.

```scala
import com.jamesward.zio_decisions.orchestration.AgentLoop

val tools = List(AgentLoop.Tool("list_orders", "Lists a customer's orders.", inputSchema))

AgentLoop.run("Which orders does C0046 have?", tools, (name, arguments) => callTool(name, arguments), llm)
  .map(result => (result.reply, result.ending, result.steps))
```

Pass `documents` (skills, references) and the decision model can also choose to read one, offered by its title, description and
metadata. A document read is kept in the loop's state for the decision model and given to the generative model, and the documents it
links to become choices in turn, so guidance loads progressively, at most `Config.maxReadings` per run.

`Config.strategy` (`AgentLoop.Strategy`, each off by default) changes how the decision model is asked, to save tokens and
generative-model calls: `boundCalls` offers calls whose arguments an earlier result holds (a search hit's `filePath`
for a tool taking `file_path`) ready-made, so choosing one needs no generative model; `noRepeats` doesn't make a call
that already succeeded again; `relevanceFilter` first asks, with one yes-or-no question per tool in a single request,
which tools the request could need, and offers only those (with an option for the rest); `catalogInState` describes
the tools once, in the state, rather than in every option; `parallelCalls` also asks for a second, independent call
to run at the same time; `context` (`keepContext`, `rateContext`) has the decision model judge, with each decision, which earlier
results stay in context (a yes-or-no question, or a 0–3 rating, per earlier step with a long result): one judged no
longer needed leaves the decision model's next states and the generative model's prompts, while the loop's `steps` (the transcript,
the record) keep it whole. `correct` is course correction: before each decision, one decision-model request assesses the newest check (a test run, a
build, a command: `Config.isCheck`): did it show the work working, does its failure repeat an earlier one, does it say
why; after a failed check the decision also offers moves (diagnose, reread the spec, a different approach, escalate to
`Config.escalation`), whose guidance stays in the context until a check passes. `giveUp` adds, from the third failed
check, whether the work is getting closer; when the decision model judges it isn't, only the endings are offered. Arguments that don't
fit the tool's schema are written again, once, told what's wrong. The loop's decision-model and generative-model calls retry transient failures (a reset connection,
throttling, a failing server) twice. `run`'s `live` is read again at every decision and added to the context: what's
changing around the loop while it runs.

`AgentLoopSpec` covers the option set, the steps and documents the decision model sees, failures, the budgets and the endings.

### Parallel parts

`orchestration.ParallelLoop` works on a request made of independent parts (scenarios, endpoints, functions, changes)
with one `AgentLoop` per part at the same time, coordinated through one append-only log of every loop's tool calls
rather than separate copies of the work to reconcile afterwards:

1. gather: one loop reads what the request needs known (the spec, the files it names) and changes nothing.
2. plan: the generative model (`ParallelLoop.Planner`, besides `Llm`), given the request and what was gathered,
   proposes the parts and any setup they all need; the decision model answers, in one yes-or-no question, whether they can be done at
   the same time. One part, or a no, runs the request as one `AgentLoop` that starts from what was gathered.
3. setup, when planned: one loop makes the changes every part needs first.
4. fork: a loop per part, each told its part and the others'. A part that doesn't finish (unable, out of steps, its
   loop failed) gets another round (`Config.retries`), all such parts at the same time, from its first attempt's calls.
5. join: one loop checks the parts together (the whole build and test suite) and replies.

Every loop's context is a projection of the log (`ParallelLoop.view`, pure), read again at each decision: the gathered
and setup work, a retried part's earlier attempt, and the other parts' calls (edits above reads), as `Sections`. Each
loop first asks the decision model which tools its task needs (`Config.filterTools`). `ParallelLoop.currentLoop` (a `FiberRef`) names
the loop a tool call is for, and log lines carry `loop=<name>`, so the host's `Tools` can coordinate (serialize edits,
merge test runs). `Result.events` is the log; `Result.parts` each part's ending and time. Interrupting the run
interrupts every loop. `ParallelLoopSpec` checks the phases, that the parts run at the same time (and, in simulated
time, that the run takes the gathering plus the longest part plus the join), failure isolation, retries, interruption,
tool filtering, the views and plan normalization.

`ParallelLoop.auto` is the entry point that needs no mode: the decision model first answers whether the request is several
independent parts of real work (one cheap request); when not, it's one `AgentLoop` from the start (no gathering, no
planning); when so, it's `run` with `Config.draftFirst`: after the plan, one loop drafts every part in one pass and
runs the tests once, the decision model judges each part from that run's results (`ParallelLoop.digest` keeps the per-test lines and
the summary of a long output), and only the parts not done are forked. When most failed and the decision model sees one shared cause in the draft's run, a repair loop
fixes that once, and the parts are judged again before any is forked.

### Context as sections

`orchestration.Sections` is the context algebra both loops use: titled sections with a priority and a floor, combined
by concatenation (a monoid) and fitted to a budget by `fit`, which cuts lower-priority, older sections first, each to
its floor, and drops them only once all are at their floor. `SectionsSpec` checks its laws as properties: bounded,
identity under budget, idempotent, monotone in the budget, priority-ordered. `AgentLoop.Config.requestBudget` bounds a
whole decision-model request (the options as they are; the steps and the context fitted to what's left), `llmContextBudget` the
generative model's context, and a request the decision model still rejects as too long is made again smaller.

### The process tree

`orchestration.ProcessTree` is the agent's work as a tree of processes: tasks, the loops working on them, their tool
calls, each with its state (running, done, failed, stopped), its times and a way to stop it. It travels on the fiber
(`ProcessTree.current`): `ProcessTree.run(kind, name)(zio)` registers `zio` under the fiber's current node, so
`AgentLoop`'s tool calls and `ParallelLoop`'s loops register themselves in whatever tree their task runs in, without
being handed it. `stop(id, reason)` interrupts a node and everything below it: a stopped tool call is a failed step
(its loop goes on), a stopped part is that part's outcome (not tried again), a stopped task ends. `stalled(after)`
lists the running nodes with no sign of life for that long (loops report one at each decision), `render` gives the
tree as text, and ended nodes are pruned to the last 400. `ProcessTreeSpec` covers nesting, outcomes, a hung call
stopped mid-loop, stopping a subtree, rendering and pruning.

`orchestration.Overseer` is the decision model as the tree's supervisor: `stalled(tree, after)` gives the calls running that long
with no sign of life, each with its history (the durations of ended calls of the same tool, and of the same tool with
the same arguments' start), the task it's for and how often it was already judged worth waiting for; `judge` asks
the decision model, in one request for all of them, to keep waiting or stop each. `OverseerSpec` covers both.

### Messages while the agent works

`orchestration.Interjection.route(message, running)` asks the decision model, in one choice, what a user's message that arrives while
tasks run is: `NewTask` (a request of its own, to start alongside them), `Steer(task)` (it adds to, corrects or
redirects that task: give it to the task's loop through `AgentLoop.run`'s `live`), `After(task)` (a follow-up that
needs that task's result), `Stop(task)` (stop it, with nothing to do instead), `Status` (a question about the
work in progress), or `StopProcess(id)` (stop one of the running processes it's given, a tool call or a loop). The decision model sees the message and each running task's request, time, calls so far and earlier
steering. `InterjectionSpec` covers the options and the routes.

### Notices between loops

`orchestration.Notices` pushes what one loop does to the loops it bears on, as it happens. A host publishes
`Notice`s (by which loop, a kind, subjects such as files, and text) on a `Notices.Bus`, from its own record of the work
(soness follows its event log), and sets the bus in `Notices.bus` for the loops' fibers. A loop run with
`Config.notices` other than `Off` opens an `Inbox` for its loop id (`ParallelLoop.currentLoop`); the host adds the
loop's interests as it works (`Notices.interest`, the files a call names, say), and a notice from another loop about
one of them reaches it:

- `Note`: it's in the context of the loop's next decision;
- `Judge`: the decision model also judges it, one yes-or-no question with the loop's request, what it's doing now
  and what it has touched, and one that makes that work out of date is marked so at the top of the next decision;
- `Interrupt`: as `Judge`, and such a notice also stops the work in flight (the arguments' writing, a call of a tool
  that changes nothing), a failed step that says why. An edit is never stopped.

A failure that starts with `Notices.OutOfDate` (that stop, or a host's own, such as an edit refused because its file
changed since it was read) isn't counted against its arguments: the same call may be made again. `NoticesSpec` covers
the policies.

## Provider-neutral orchestration

The same `zio-decisions` artifact includes the `com.jamesward.zio_decisions.orchestration` package: neutral operation schemas, a workflow AST and runtime, plan-before-execute and runtime-checkpoint decision controllers, schema-safe filtering, checkpoint recovery, evidence handling, metrics, audit snapshots, and compact exchange logging. It depends only on the `DecisionModel` trait, not on a decision provider, a generative model provider, MCP implementation, Javadocs, Maven, or any application domain.

Applications supply three boundaries:

1. `Vector[OperationSpec]` describes the available operations using JSON input/output schemas.
2. `OperationInvoker` executes one schema-valid operation call.
3. For summarized `runMode` executions, `LlmTransport` implements argument extraction, optional filter fallback, and final summarization using the application's chosen model provider.

The orchestrator itself uses `Decisions.loop`; it does not implement a separate decision loop.

### Catalog and adapters

An `OperationSpec` contains only a name, description, input schema, output schema, and `CapabilityKind`:

```scala
import com.jamesward.zio_decisions.orchestration.*
import com.jamesward.zio_decisions.orchestration.runtime.OperationInvoker
import zio.*
import zio.json.ast.Json

val querySchema = Json.Obj(
  "type" -> Json.Str("object"),
  "properties" -> Json.Obj(
    "query" -> Json.Obj("type" -> Json.Str("string")),
  ),
  "required" -> Json.Arr(Json.Str("query")),
  "additionalProperties" -> Json.Bool(false),
)

val resultSchema = Json.Obj(
  "type" -> Json.Str("object"),
  "properties" -> Json.Obj(
    "items" -> Json.Obj(
      "type" -> Json.Str("array"),
      "items" -> Json.Obj(
        "type" -> Json.Str("object"),
        "properties" -> Json.Obj(
          "id" -> Json.Obj("type" -> Json.Str("string")),
        ),
      ),
    ),
  ),
)

val externalOperations = Vector(
  OperationSpec(
    name = "enumerate",
    description = "Enumerate records matching a query.",
    inputSchema = querySchema,
    outputSchema = resultSchema,
    kind = CapabilityKind.Mcp,
  ),
)

val operationInvoker = new OperationInvoker:
  def call(operation: String, arguments: Json.Obj): Task[Json] =
    executeInApplication(operation, arguments)
```

`CapabilityKind.Mcp` denotes an externally invoked catalog operation; it does not introduce a dependency on an MCP library. An application may adapt any provider or local capability into this interface. Provider-specific result normalization, authentication, transport retries, and schema discovery remain outside this artifact.

`Catalog.initialInputSchema(operations)` builds a synthetic capability's host-input surface solely from the supplied external operation input schemas. Fields are deterministic and optional, incompatible definitions for the same name are preserved with `anyOf`, internal capabilities are ignored, and `additionalProperties` is false. This lets an outer agent supply any currently known operation arguments without a domain-specific input contract.

Normal summarized orchestration also needs the three internal capability specifications and an `InternalLlmInvoker`:

```scala
val catalog = externalOperations ++ Vector(
  Catalog.extract,
  Catalog.synthesizeFilter,
  Catalog.summarize,
)

for
  internalLlm <- InternalLlmInvoker.make(applicationLlmTransport)
  result <- GenericOrchestrator.runMode(
    prompt = "Inspect the relevant records and explain the result.",
    catalog = catalog,
    operationInvoker = operationInvoker,
    internalLlm = internalLlm,
    mode = OrchestrationMode.NoPlan,
    config = OrchestrationConfig(),
  )
yield result.finalText
```

`LlmTransport` is a capability SPI, not a provider client:

```scala
trait LlmTransport:
  def extract(request: ExtractionRequest): Task[LlmResult[Json.Obj]]
  def synthesizeFilter(
    request: PredicateFilter.SynthesisRequest,
  ): Task[LlmResult[Json.Obj]]
  def summarize(prompt: String, evidence: Json.Arr): Task[LlmResult[String]]
```

The host validates every returned extraction and predicate against the relevant runtime schema before using it.

### Execution modes

`OrchestrationMode` has four stable wire names:

| Mode | Wire name | Controller behavior | Runtime filter behavior |
| --- | --- | --- | --- |
| `Plan` | `plan` | The decision model builds a symbolic workflow before execution. | The current plan executor creates filter synthesis before runtime variants are available, so filtering currently uses `LlmTransport`. |
| `NoPlan` | `no-plan` | The decision model chooses and executes one action from each actual runtime checkpoint. | Decision-first routing uses runtime variants up to the configured threshold, then LLM predicate fallback. |
| `PlanLlmFilters` | `plan-llm-filters` | Same plan-before-execute controller. | Explicit label for the plan arm's current LLM-filter behavior. |
| `NoPlanLlmFilters` | `no-plan-llm-filters` | Same runtime-checkpoint controller. | Configure `decisionFilterVariantThreshold = 0` to force LLM predicate synthesis. |

A typical experiment configuration makes the backend distinction explicit:

```scala
val config = OrchestrationConfig(
  decisionFilterVariantThreshold =
    if mode.forceLlmFilters then 0 else 1024,
)
```

`run` is a compatibility convenience for `runMode(..., OrchestrationMode.Plan, ...)`.

In plan mode, independent ready steps execute in dependency waves. A failed call retries only that call within the shared recovery budget; completed dependencies and successful fanout siblings are retained. Recoverable filter outcomes continue deterministically from the retained complete source without installing a false replan, so the current implementation reports `replans = 0`.

In no-plan mode, the selected action executes inside the `Decisions.loop` handler before the next decision. Confirmed action fingerprints suppress duplicates. A terminal operation failure is compacted into the next decision checkpoint, the failed invocation is retired, and no failed workflow step is committed. The decision model can then choose another operation or terminate with retained evidence.

### Evidence-only nested-tool terminal

Use `GenericOrchestrator.runEvidence` when an outer agent or application should consume structured evidence instead of an inner summary. Its default mode runs no LLM inside the decision loop:

```scala
val initialInputSchema = Json.Obj(
  "type" -> Json.Str("object"),
  "properties" -> Json.Obj(
    "query" -> Json.Obj("type" -> Json.Str("string")),
  ),
  "required" -> Json.Arr(Json.Str("query")),
  "additionalProperties" -> Json.Bool(false),
)

val initialInput = Json.Obj("query" -> Json.Str("target"))

val evidenceProgram = GenericOrchestrator.runEvidence(
  prompt = "Inspect the records relevant to the request.",
  catalog = externalOperations,
  initialInput = initialInput,
  initialInputSchema = initialInputSchema,
  operationInvoker = operationInvoker,
  config = OrchestrationConfig(decisionFilterVariantThreshold = 1024),
).map(_.evidence)

// evidenceProgram: ZIO[DecisionModel, Throwable, Json.Arr]
```

`runEvidence` has these stricter guarantees:

- Initial input is validated before any operation or decision request.
- Only input fields actually present at runtime become bindable.
- Only external operations whose required arguments bind exactly from supplied input or prior typed outputs are offered.
- Extraction and summary capabilities are always removed from the catalog and cannot run.
- `EvidenceFilterMode.Decisions` is the default. Filtering uses decision-model field selection and variant classification; exceeding `decisionFilterVariantThreshold` fails with `DecisionFilterCapacityExceeded` rather than invoking an LLM.
- `EvidenceFilterMode.Llm` forces runtime filtering through the supplied `InternalLlmInvoker`. Only predicate synthesis is called; extraction and summarization remain disabled.
- The decision model terminates with `return_evidence`, and `OrchestrationEvidenceResult` contains structured evidence rather than final prose.
- Filtered evidence replaces the corresponding unsafe raw producer evidence.
- If an output and a later input are type-compatible but do not bind exactly by declared name/metadata, the orchestrator does not guess or relabel them. A caller may use returned evidence to supply an additional input in a later invocation.

The explicit LLM-criteria variant is configured as follows:

```scala
for
  filterLlm <- InternalLlmInvoker.make(applicationLlmTransport)
  result <- GenericOrchestrator.runEvidence(
    prompt = "Inspect the records relevant to the request.",
    catalog = externalOperations,
    initialInput = initialInput,
    initialInputSchema = initialInputSchema,
    operationInvoker = operationInvoker,
    config = OrchestrationConfig(),
    filterMode = EvidenceFilterMode.Llm,
    filterLlm = Some(filterLlm),
  )
yield result.evidence
```

The default decision mode is suitable for exposing a configured decision loop as one tool inside an outer LLM loop without nesting another LLM: the outer model supplies inputs and writes the final answer, while the inner loop performs only decisions, deterministic host work, and external operation calls. The LLM mode is an explicit comparison arm that changes only filter-criteria generation.

### Schema binding and fanout safety

Bindings are conservative and deterministic. A required field binds only from a compatible declared value with the same exposed name. Schema descriptions may constrain producer/consumer operation relationships, but descriptions do not authorize arbitrary semantic relabeling. Runtime-confirmed singleton arrays may expose fixed-index projections; empty and multi-item arrays are never promoted this way.

Fanout is rejected unless the host can prove one of:

- the source came from a `Ready` filter; or
- its declared `maxItems` is at or below `hostGuard` and the producer output passed local schema validation.

Raw unsafe arrays fail before any downstream call. `hostGuard`, routing thresholds, attempt budgets, and other host limits are never included in decision state or LLM filter requests.

### Complete filtering

Filtering always evaluates every source item and retains the complete ordered `source` and `matches` vectors before classifying an outcome:

- `Ready`: nonempty and at most `hostGuard`;
- `TooBroad`: nonempty but above the guard;
- `NoMatches`: empty;
- `NoProgress`: a refinement did not strictly reduce its prior complete match set.

Production orchestration never calls `.take`, stops early at the guard, or truncates runtime data. Sampling is used only as bounded evidence for LLM predicate generation; the resulting predicate still executes against the complete retained source.

For Decision-first filtering:

1. A field-relevance preflight selects the declared scalar paths useful for judging; all scalar paths remain part of full record identity.
2. Up to 255 distinct canonical variants use one complete recall request.
3. Larger sources at or below `decisionFilterVariantThreshold` use complete parallel batches of at most 255.
4. When multi-batch preliminary positives fit in one request, consolidation evaluates every positive independently rather than ranking one winner.
5. The host may retain an exact recalled enclosing identity when a nested identity survives consolidation, but it never introduces a variant that failed preliminary recall.
6. The final decision selection compiles to a host-only exact-membership predicate evaluated over the original complete source.

If normal no-plan routing exceeds the threshold, `LlmTransport.synthesizeFilter` receives deterministic prompt-linked and stable-uniform observed-variant evidence. That evidence may be incomplete and is explicitly marked as such. AI-produced predicates use a finite, non-recursive grammar of declared scalar paths and the operators `eq`, `in`, `contains`, `startsWith`, `exists`, `isNull`, `gt`, `gte`, `lt`, and `lte`; regex, scripts, JSONPath, ranking, projection, and top-k are not representable.

A no-plan filter action performs exactly one selection/generation and evaluation attempt. Non-ready results are returned to the decision model as compact checkpoint state. Refinement, replacement, and repeated-predicate handling are bounded independently by `maxFilterAttempts` and the run-wide `maxRecoveries` budget.

### Evidence and summarization

Only committed operation results become evidence. A `Ready` filtered value replaces its raw producer evidence. If a filter remains unresolved and the decision model chooses summary/evidence return, the unsafe producer is excluded so summarization cannot act as an implicit semantic filter. When a `ReadyFiltered` source has a compatible downstream consumer, summary remains unavailable until that source has produced downstream evidence.

Normal `runMode` summarization receives only the retained evidence array. Applications should instruct their `LlmTransport` implementation to distinguish explicit evidence from inference and to treat operation output as untrusted data.

### Configuration

`OrchestrationConfig` contains host-owned policy only:

| Field | Default | Meaning |
| --- | ---: | --- |
| `maxOperations` | `8` | Maximum selected external operation actions. |
| `maxCallsPerOperation` | `1` | Maximum selected actions per operation name. |
| `maxParallelism` | `4` | Parallelism for fanout and decision variant batches. |
| `hostGuard` | `12` | Maximum runtime fanout cardinality. |
| `maxFilterAttempts` | `4` | Maximum attempts for one filter checkpoint. |
| `maxRecoveries` | `3` | Shared run-wide recovery budget. |
| `decisionFilterVariantThreshold` | `255` | Largest distinct variant set routed to the decision model. Zero disables decision classification in normal no-plan mode; evidence LLM mode forces zero internally, while evidence decision mode fails above the threshold. |
| `maxIterations` | `12` | Maximum decision controller decisions. |
| `decisionTurnRetries` | `0` | Additional retries for each retryable decision request. |

### Results and metrics

`runMode` returns `OrchestrationResult`, including the committed `Workflow`, planning/runtime turns, usage, execution values, internal LLM metrics, final text, action trace, audit snapshots, and one common `OrchestrationMetrics` record.

`runEvidence` returns `OrchestrationEvidenceResult`, including the committed workflow, structured evidence, decision turns/usage, execution report, action trace, audit snapshots, and the same common metrics shape. Extraction and summary metrics remain zero by contract. LLM-filter metrics are zero in `EvidenceFilterMode.Decisions` and record only predicate-synthesis calls in `EvidenceFilterMode.Llm`.

The common metrics separate:

- logical decision controller turns, tokens, and elapsed request time;
- physical decision attempts, successes, failures, and time;
- internal decision-filter calls, tokens, and time;
- extraction, LLM-filter, and summary calls/tokens/time;
- physical external-operation calls, successes, failures, merged wall time, and summed call time;
- recoveries, actual replans, and directly measured total time.

Physical external-operation metrics are available when the supplied invoker also implements `McpPhysicalMetricsProvider`; otherwise those fields are zero. `totalTimeMs` is measured directly rather than derived by subtracting overlapping timers.

For compact observation, decorate the decision model layer:

```scala
val model = TypeSafe.live >>> DecisionModel.observed(
  ExchangeLogger.selected(fullBodies = false),
)
```

Compact logs retain decision metadata and schema descriptions while omitting runtime arrays and exact membership fingerprints. Full exchange bodies are intended only for temporary diagnosis because they may contain application state and model output.
