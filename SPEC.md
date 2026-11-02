# zio-decisions

A Scala 3 / ZIO library for decision models ("System One" models): models
that answer typed, atomic questions about a piece of state with calibrated
probabilities, instead of generating text. One provider-neutral API, with
providers for:

| Provider | Object | Endpoint |
| --- | --- | --- |
| TypeSafe AI's Jev / System One | `typesafe.TypeSafe` | `POST https://api.typesafe.ai/v1/systemone` |
| OpenAI's Decisions API | `openai.OpenAI` | `POST https://api.openai.com/v1/decisions` |

## Goals

1. **Provider-neutral.** Everything (`ask`, `askDynamic`, `loop`, the
   `orchestration` package) is written against one trait,
   `DecisionModel`; a provider is a `ZLayer[Client, _, DecisionModel]`.
   Switching providers is changing one layer. A model of your own, or a
   test double, is another implementation of the trait.
2. **Typed, atomic questions.** Exactly three question shapes —
   `Question.Noul` ("is this true?"), `Question.Choice` ("which of
   these?"), `Question.Score` ("which level?") — modeled as a closed
   `sealed trait Question[S]` with one generic class per shape, `S`
   being the type of that question's `instructions`. Every provider maps
   them onto its own wire (TypeSafe natively; OpenAI as `predicate`,
   `choice`, `score`).
3. **NamedTuple-driven request/response symmetry.** `ask(state, questions)`
   takes a `NamedTuple` of `Question`s; `.answers` / `.run` return a
   `NamedTuple` with the *same field names*, each typed per question
   (`AnswerOf[Question.Noul[?]] = Probability`, `AnswerOf[Question.Choice[?]]
   = ChoiceAnswer`, `AnswerOf[Question.Score[?]] = ScoreAnswer`) via the
   `AnswerOf` / `AnswersOf` match types. No stringly-typed answer-map
   lookups anywhere in the public API.
4. **Illegal states unrepresentable.**
   - `ChoiceCriteria` (1–255 options) and `ScoreCriteria` (2–10 levels)
     are built only through smart constructors returning
     `Either[String, _]`.
   - `Probability` is an opaque `Double` provably within `[0.0, 1.0]`;
     every probability and confidence a provider returns is one. A value
     outside that range fails closed as `Error.MalformedAnswer`, never
     silently clamped.
5. **Typed `ask` state and instructions are not wrapped.** `ask` and every
   `Question` factory are generic in a `Schema` type parameter
   (`ask[S: Schema](state: S, ...)`, `Question.Noul[S: Schema](instructions:
   S, ...)`) and keep `S` as a real type parameter on the built value.
   Only heterogeneous criteria descriptions need the `Content` erasure
   boundary; the loop API also uses `Content` for state views, options,
   choice instructions, observations, and audit snapshots.
6. **No `DynamicValue`.** The JSON erasure boundary is `zio.json.ast.Json`.
7. **Builder/service split.** `ask(...)` returns a pure-data
   `AskRequest[Names, Values]`; `.answers` / `.run` require a
   `DecisionModel` in the environment, the only thing that touches a wire.
8. **One error type.** `Decisions.Error` covers every provider: HTTP
   statuses map onto the same cases, `TooLong` is a request over the
   model's context, `Refused` a question the model declined, and
   `Error.isTransient` says what's worth retrying.
9. **One observation shape.** `Middleware` and `ExchangeObserver` see each
   exchange's typed `DecisionModel.Request` / `Decision` and their
   provider-neutral canonical JSON, whichever provider answered.
10. **Mockable.** `TypeSafeMock(behaviors*)` (in `src/test`) scripts Jev's
    System One wire below a real `TypeSafe` model; one behavior scripts
    one whole request.

## Non-goals

- Model listing endpoints.
- Request-level sampling/inference controls — decision endpoints have none.
- Streaming — both endpoints are a single JSON request/response.
- OpenAI image inputs (`input_image`) — the shared `input` is the state, as
  text (a string as itself, any other value as its JSON).

## Why `Question` is a `sealed trait`, not an `enum`

Scala 3 widens an `enum` case's inferred type to the enum type itself
wherever it isn't otherwise constrained — including a bare `NamedTuple`
field, which is exactly the position `ask`'s `Values` type parameter is
inferred from. That widening would collapse every question to plain
`Question[?]` and leave `AnswerOf` unable to pick a branch. A `sealed
trait` + plain classes carries its own precise type at that call site
(`ask(..., (isUrgent = Question.Noul("...")))` infers that field as
`Question.Noul[String]`), with identical call-site syntax
(`Question.Noul(...)` still works via the companion object) and
identical exhaustive-match support.

## Why `Question.Noul` / `Choice` / `Score` are generic in `S`

`instructions` is required to have *some* `Schema` — any type works, not
just `String` — so the natural signature is `Question.Noul[S: Schema]
(instructions: S, ...)`. Keeping `S` as the class's own type parameter
(rather than converting to an erased `Content` at construction) means
`question.instructions: S` is the exact value the caller passed, with no
decode step and no possible failure — there's nothing to get wrong,
because nothing was thrown away. The captured `Schema[S]` rides along as
a private field so `internal.Canonical` can still encode `instructions` to JSON
later, at the one point that actually needs a JSON representation
(building a provider's request) — never earlier, and never exposed on the
type itself.

This works because Scala recovers the `S` ↔ `Schema[S]` pairing through
an ordinary pattern match: `case n: Question.Noul[s] => ...` binds a
fresh existential type variable `s`, and both `n.instructions: s` and
`n.schema: Schema[s]` refer to the *same* `s` inside that branch, so
`Codecs.toJsonAst(n.instructions)(using n.schema)` type-checks soundly
with no cast.

## Why `zio.json.ast.Json`, not `DynamicValue`

`zio.schema.DynamicValue` (annotated `@directDynamicMapping`) is
`zio-schema`'s own built-in mechanism for "opaque, already-JSON-shaped
passthrough" inside a `derives Schema` case class — it exists
specifically for this scenario. Using it would have meant keeping the
whole request (`state`, `model`, `questions`) as one derived-`Schema`
value, the way `zio-bedrock-converse`'s wire types are.

This library doesn't need that, because — unlike the response, which
stays a plain `derives Schema` case class end to end (`Answer` / `Usage`
/ `SystemOneResponse` for TypeSafe, `zio-json`-derived decoders for OpenAI) — the *request*'s
dynamic fields are always encoded from a concretely-known `Schema[S]` at
the exact point of construction. So instead of merging everything into
one derived request `Schema`, `internal.Canonical` encodes `state` and
each question's `instructions` **independently**, each via its own
`Schema[S]` (`Codecs.toJsonAst`, which round-trips through
`zio-schema-json`'s binary codec and `zio-json`'s own parser), and
composes the pieces with `zio-json`'s own `Json.Obj` / `Json.Arr`
constructors. Providers render the final tree with `zio-json`'s own
printer (`.toJson`) — real, tested code from both libraries, no
hand-rolled JSON string escaping anywhere.

The result: `zio.schema.DynamicValue` does not appear anywhere in this
codebase. `Content` wraps `zio.json.ast.Json` wherever a stable serialized
erasure boundary is needed: heterogeneous criteria descriptions in typed
questions, and loop state views, option/choice instructions, observations,
and audit snapshots.

## Authentication & configuration

| Provider | `live` reads | Required | Default |
| --- | --- | --- | --- |
| `TypeSafe` | `TYPESAFE_API_KEY` | yes | — |
| `TypeSafe` | `TYPESAFE_DEFAULT_MODEL` | no | `jev-latest` |
| `OpenAI` | `OPENAI_API_KEY` | yes | — |
| `OpenAI` | `OPENAI_DECISIONS_MODEL` | no | `gpt-6-luna` |

Both are bearer-token authenticated. `layer(apiKey, model, endpoint)`
builds either explicitly, against any origin speaking the same wire (a
proxy, or a test double). A missing key fails `live` with
`Error.MissingApiKey(variable)`.

## Public API surface

```scala
package com.jamesward.zio_decisions

trait DecisionModel:
  def provider: String                       // "typesafe", "openai"
  def defaultModel: ModelId
  def name: String                           // "typesafe:jev-latest"
  def decide(request: DecisionModel.Request): IO[Error, DecisionModel.Decision]

object DecisionModel:
  final case class Request(state: Content, questions: List[(QuestionId, Question[?])], model: Option[ModelId] = None):
    def json(defaultModel: ModelId): Json    // canonical
  final case class Decision(answers: Map[QuestionId, DynamicAnswer], model: ModelId, usage: Usage):
    def json: Json                           // canonical
  def decide(state: Content, questions: List[(QuestionId, Question[?])]): ZIO[DecisionModel, Error, Decision]
  def observed(observer: ExchangeObserver): URLayer[DecisionModel, DecisionModel]
  extension [R, E](layer: ZLayer[R, E, DecisionModel]) def @@(m: Middleware): ZLayer[R, E, DecisionModel]

object Decisions:
  opaque type ModelId    = String
  opaque type QuestionId = String            // NamedTuple key ↔ wire question id ↔ answer key
  opaque type Probability = Double           // always in [0.0, 1.0]

  final class Content                        // wraps a zio-json Json node
  final class NoulCriteria / ChoiceCriteria / ScoreCriteria   // smart-constructed
  sealed trait Question[S]                   // Noul[S] | Choice[S] | Score[S]

  case class ChoiceAnswer(choice: String, probabilities: Map[String, Probability], confidence: Probability)
  case class ScoreAnswer(score: Double, legend: Map[Int, String], probabilities: Map[Int, Probability], confidence: Probability)
  enum DynamicAnswer { case Noul(p: Probability); case Choice(a: ChoiceAnswer); case Score(a: ScoreAnswer) }
  type AnswerOf[Q]; type AnswersOf[Hs <: Tuple]; sealed trait AllQuestions[Hs <: Tuple]

  case class Usage(inputTokens: Int, outputTokens: Int)
  case class Result[+T](answers: T, model: ModelId, usage: Usage)

  final class AskRequest[Names <: Tuple, Values <: Tuple]:
    def model(m: ModelId): AskRequest[Names, Values]
    def answers: ZIO[DecisionModel, Error, NamedTuple.NamedTuple[Names, AnswersOf[Values]]]
    def run:     ZIO[DecisionModel, Error, Result[NamedTuple.NamedTuple[Names, AnswersOf[Values]]]]
  final class DynamicAskRequest:             // runtime-sized: Map[QuestionId, DynamicAnswer]

  inline def ask[S: Schema, Names <: Tuple, Values <: Tuple](state: S, questions: NamedTuple.NamedTuple[Names, Values])
      (using inline ev: Values <:< NonEmptyTuple, inline all: AllQuestions[Values]): AskRequest[Names, Values]
  def askDynamic[S: Schema](state: S, questions: Iterable[(QuestionId, Question[?])]): Either[String, DynamicAskRequest]
  def loop / loopWithTurn                    // see "Loop API"

  enum ExchangeOutcome { case Success(response: Json); case Failure(cause: Cause[Error]) }
  case class ExchangeObservation(request: Json, outcome: ExchangeOutcome, latencyMs: Long)
  trait ExchangeObserver                     // none, fromFunction, logging

trait Middleware:                            // `left ++ right` applies left around right; `@@` = `++`
  def apply(request: Middleware.Request, next: IO[Error, Middleware.Response]): IO[Error, Middleware.Response]
  def layer: URLayer[DecisionModel, DecisionModel]
object Middleware:                           // make, observe, logging(LoggingConfig), identity

package typesafe
object TypeSafe:
  opaque type ApiKey = String
  val JevLatest: ModelId
  def layer(apiKey: ApiKey, model: ModelId = JevLatest, endpoint: URL = DefaultEndpoint): URLayer[Client, DecisionModel]
  val live: ZLayer[Client, Error, DecisionModel]

package openai
object OpenAI:
  opaque type ApiKey = String
  val DefaultModel: ModelId                  // gpt-6-luna
  def layer(apiKey: ApiKey, model: ModelId = DefaultModel, endpoint: URL = DefaultEndpoint): URLayer[Client, DecisionModel]
  val live: ZLayer[Client, Error, DecisionModel]
```

## Provider mappings

**TypeSafe (Jev).** The request is the canonical JSON as is
(`{state, model, questions: {id: {type, instructions, criteria}}}`); the
response's `answers` map is keyed by question id. A 400 whose body says
`max_tokens_exceeded` is `Error.TooLong`.

**OpenAI.** The state is the shared `input`, as text. Each question becomes
a named question:

| Library | OpenAI |
| --- | --- |
| `Noul(instructions, criteria)` | `predicate`; criteria appended as `True when: …` / `False when: …` |
| `Choice(instructions, criteria)` | `choice`; each option `{value, description}` (its value when it has no description) |
| `Score(instructions, criteria)` | `score`; levels `{label: "Level i", description}`; the answer's legend is the levels' descriptions |

Answers are matched by `name`; a `refusal` is `Error.Refused(id)`; a 400
saying `context_length_exceeded` / too long is `Error.TooLong`. Structured
(non-string) instructions and descriptions are sent as their JSON text.

## Loop API

`loop` and `loopWithTurn` present a host-generated action set as one
`Choice` per iteration to the environment's `DecisionModel`. The builder
has copy-style `.model`, `.maxIterations`, `.choiceInstructions[I: Schema]`,
`.retryEachTurn(LoopRetryPolicy)`, `.observe(LoopObserver[E, O])`, `.run`
and `.runAudited`.

```scala
case class LoopRetryPolicy(maxRetries: Int, initialDelay: Duration = 500.millis)
case class LoopAuditOption(id: String, description: Content)
case class LoopAuditTurn(iteration: Int, stateView: Content, options: List[LoopAuditOption])
case class AuditedLoopResult[+O](result: LoopResult[O], audit: List[LoopAuditTurn])

enum LoopObservation[+E, +O]:
  case OptionsGenerated(iteration: Int, stateView: Content, options: List[LoopAuditOption])
  case Decision(turn: LoopTurn)
  case Completion(result: LoopResult[O])
  case Failure(cause: Cause[Error | E], turns: List[LoopTurn])
```

Observers are best-effort: callback defects and interruption can't change
the result. Exchange events contain no headers or tokens but do contain
potentially sensitive full bodies. Semantic ordering is validated options →
options event → decision → decision event → handler → recursion/completion.
Per-turn retries apply only to the decision request and only for
`RateLimit`, `ServiceOverloaded`, and `InternalServer`; default is zero.

## Errors

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
  final case class MalformedAnswer    (questionId: QuestionId, message: String) extends Error
  final case class MissingAnswer      (questionId: QuestionId)          extends Error
  final case class Refused            (questionId: QuestionId)          extends Error
  final case class InvalidLoop        (message: String)                 extends Error
  final case class MaxIterations      (iterations: Int)                 extends Error

  def isTransient(error: Error): Boolean     // Transport / RateLimit / ServiceOverloaded / InternalServer
  extension [R, A](zio: ZIO[R, Error, A])
    def retryOnRetryable: ZIO[R, Error, A]   // RateLimit / ServiceOverloaded / InternalServer, 2x backoff
```

## Dispatch model

```
ask(state, namedTupleOfQuestions).answers / .run
  │  AskRequest(state: Content, model override, entries in NT order)
  ▼
RequestImpl: DecisionModel.decide(Request(state, entries, model))
  │                        ┌─ TypeSafe: Canonical.request → POST /v1/systemone → decode answers map
  ├── the environment's ───┤
  │   DecisionModel        └─ OpenAI:   Wire.Request(input, predicate|choice|score) → POST /v1/decisions
  │                                     → match answers by name (refusal → Error.Refused)
  ▼
Decision(answers: Map[QuestionId, DynamicAnswer], model, usage)
  │  for each (id, question) in NT order:
  │    missing → Error.MissingAnswer;  another kind → Error.MalformedAnswer
  ▼
Tuple in NT order, cast once to NamedTuple[Names, AnswersOf[Values]]
```

Probabilities are range-checked by the provider as it decodes
(`Error.MalformedAnswer` when out of `[0, 1]`).

## Test layout

`SharedSpec` declares two scenario lists:

- **`happyPathScenarios`** — Noul / Choice / Score / combined, asserted on
  shape plus one fact obvious enough for any live model. They're written
  against `DecisionModel`, so they run against the mock and every live
  provider.
- **`mockOnlyScenarios`** — exact-value decodes and every error path.

```
TypeSafeMockSpec        — criteria constructors + all scenarios         vs scripted Jev
TypeSafeIntegrationSpec — happyPathScenarios                           vs live Jev     (TYPESAFE_API_KEY)
OpenAISpec              — wire mapping, answers, refusal, errors        vs fake /v1/decisions
OpenAIIntegrationSpec   — happyPathScenarios, each question kind,       vs live OpenAI  (OPENAI_API_KEY)
                          a bad key, AgentLoop driven by OpenAI
```

Live specs are skipped when their key is unset. Locally, sbt-dotenv passes
`.env`'s variables (gitignored) to the forked test JVM; CI passes the same
names as secrets.

## File layout

```
src/main/scala/com/jamesward/zio_decisions/
  Decisions.scala            — questions, criteria, answers, Content, Error, ask / askDynamic / loop
  DecisionModel.scala        — the provider trait, Request / Decision, observed, @@
  Middleware.scala           — composable middleware around every exchange
  internal/
    Codecs.scala             — the zio-schema <-> zio-json bridge
    Canonical.scala          — the provider-neutral canonical request / answer JSON
    RequestImpl.scala        — ask(...).run: decide, check every answer, NamedTuple order
    LoopImpl.scala           — the decision loop
  typesafe/
    TypeSafe.scala           — Jev as a DecisionModel: layer, live, HTTP transport, decoding
    Wire.scala               — System One response types
  openai/
    OpenAI.scala             — OpenAI's Decisions API as a DecisionModel, with its Wire types
  orchestration/             — AgentLoop, ParallelLoop, GenericOrchestrator, … on any DecisionModel
```

## JSON encoding strategy

- **Requests** are never one derived `Schema`: each dynamic value (state,
  instructions, criteria descriptions) is encoded independently via its own
  `Schema[S]` (`Codecs.toJsonAst`) and composed with `Json.Obj` / `Json.Arr`
  (`Canonical`). TypeSafe sends that tree; OpenAI's provider renders it into
  its own derived `JsonEncoder` wire types.
- **Responses** are fixed-shape: TypeSafe's decode through
  `zio-schema-json` (`@discriminatorName("type")`), OpenAI's through
  `zio-json` derived decoders (`@jsonDiscriminator("type")`).
