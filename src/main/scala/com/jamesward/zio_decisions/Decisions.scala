package com.jamesward.zio_decisions

import com.jamesward.zio_decisions.internal.{Codecs, LoopImpl, RequestImpl}
import zio.*
import zio.http.Status
import zio.json.*
import zio.json.ast.Json
import zio.schema.Schema

/**
 * Provider-neutral entrypoint for decision models ("System One" models such
 * as TypeSafe AI's the decision model or OpenAI's Decisions API). A decision model answers
 * typed, atomic questions about a piece of `state` instead of generating text:
 *
 *   - [[Question.Noul]]   — "is this true?" → a calibrated [[Probability]].
 *   - [[Question.Choice]] — "which of these?" → the selected option plus
 *     the full probability distribution.
 *   - [[Question.Score]]  — "which level?" → a probability-weighted mean
 *     over an ordered scale.
 *
 * Ask one or more questions at once with [[ask]]; the `NamedTuple` of
 * questions becomes the wire request *and* the compile-time shape of the
 * typed answer — see [[AnswerOf]].
 *
 * Every request is answered by the [[DecisionModel]] in the ZIO environment;
 * providers (`typesafe.TypeSafe`, `openai.OpenAI`) supply its layer.
 */
object Decisions:

  // ---------- Opaque domain types ----------

  /** A provider's model id (`jev-latest`, `gpt-6-luna`). Not a closed enum:
    * providers ship new models. */
  opaque type ModelId = String
  object ModelId:
    def apply(s: String): ModelId = s
    extension (m: ModelId) def unwrap: String = m

  /** The `NamedTuple` key a [[Question]] is registered under in [[ask]].
    * Flows into the wire request's `questions` map key and back out as
    * the matching key in the response's `answers` map — the same
    * same-string-in-two-places pattern `ToolName` guards against in
    * `zio-bedrock-converse`. */
  opaque type QuestionId = String
  object QuestionId:
    def apply(s: String): QuestionId = s
    extension (q: QuestionId) def unwrap: String = q

  given CanEqual[ModelId, ModelId]       = CanEqual.derived
  given CanEqual[QuestionId, QuestionId] = CanEqual.derived
  given CanEqual[Status, Status]         = CanEqual.derived

  /** A probability or confidence value, always within `[0.0, 1.0]`.
    * Every `noul`, `confidence`, and per-option/per-level probability
    * the decision model returns is one of these — constructing one from an
    * out-of-range `Double` is impossible, so a protocol violation
    * surfaces as [[Error.MalformedAnswer]] at the decode boundary
    * instead of silently propagating a nonsense number. */
  opaque type Probability = Double
  object Probability:
    private[zio_decisions] def apply(value: Double): Either[String, Probability] =
      Either.cond(value >= 0.0 && value <= 1.0, value, s"probability out of range [0.0, 1.0]: $value")
    extension (p: Probability) def unwrap: Double = p
  given CanEqual[Probability, Probability] = CanEqual.derived

  /** Arbitrary JSON content — criteria descriptions accept a string, an
    * object, an array, or null, and multiple descriptions (possibly of
    * different origin types) live side by side in one [[ChoiceCriteria]]
    * / [[ScoreCriteria]], so something has to represent
    * "already-JSON-shaped value of statically unknown origin" once
    * they're combined. Wraps a `zio.json.ast.Json` node — never
    * `zio.schema.DynamicValue` — built from whatever `Schema`-having
    * value was passed, via `Codecs.toJsonAst`. See `Codecs` for why
    * `zio.json.ast.Json` rather than `DynamicValue`.
    *
    * In the typed `ask` API, `state` and every [[Question]]'s instructions
    * remain generic in their own `Schema` type and retain their precise public
    * types. Heterogeneous criteria descriptions use `Content` through
    * [[ChoiceCriteria.apply]], [[ScoreCriteria.apply]], or the `fromContent`
    * escape hatches. The loop API also uses `Content` as the intentional
    * serialized boundary for state views, option and choice instructions,
    * semantic observations, and audit snapshots. */
  final class Content private[zio_decisions] (private[zio_decisions] val json: Json):
    def as[A: Schema]: Either[String, A] = Codecs.fromJsonAst(json)

  object Content:
    def apply[A: Schema](value: A): Content = new Content(Codecs.toJsonAst(value))

  // ---------- Questions ----------

  /** Clarifying `true`/`false` descriptions for a [[Question.Noul]].
    * The criteria object itself is optional on [[Question.Noul]]; when
    * supplied, both entries are emitted and either description may be
    * JSON `null`, as allowed by TypeSafe's `EntryType`. */
  final class NoulCriteria private (val whenTrue: Content | Null, val whenFalse: Content | Null)
  object NoulCriteria:
    def apply(whenTrue: String | Null = null, whenFalse: String | Null = null): NoulCriteria =
      fromContent(
        whenTrue  match { case null => null; case s => Content(s) },
        whenFalse match { case null => null; case s => Content(s) },
      )
    def fromContent(whenTrue: Content | Null = null, whenFalse: Content | Null = null): NoulCriteria =
      new NoulCriteria(whenTrue, whenFalse)

  /** The named options of a [[Question.Choice]]. Between 1 and 255
    * entries — enforced at construction, so an out-of-range `Choice`
    * can never be built. Build via [[ChoiceCriteria.apply]] for
    * plain-text descriptions, or [[ChoiceCriteria.fromContent]] for
    * structured (object/array) ones. A `null` description means "the
    * option name is self-explanatory". */
  final class ChoiceCriteria private (val options: Map[String, Content | Null]):
    def size: Int = options.size

  object ChoiceCriteria:
    val MinOptions = 1
    val MaxOptions = 255

    def apply(options: (String, String | Null)*): Either[String, ChoiceCriteria] =
      fromContent(options.map((name, desc) =>
        name -> (desc match { case null => null; case s => Content(s) })
      ).toMap)

    def fromContent(options: Map[String, Content | Null]): Either[String, ChoiceCriteria] =
      if options.size < MinOptions then
        Left(s"Choice.criteria needs at least $MinOptions option, got ${options.size}")
      else if options.size > MaxOptions then
        Left(s"Choice.criteria supports at most $MaxOptions options, got ${options.size}")
      else
        Right(new ChoiceCriteria(options))

  /** The ordered levels of a [[Question.Score]], low to high. Between 2
    * and 10 levels — enforced at construction. Build via
    * [[ScoreCriteria.apply]] for plain-text or `null` levels, or
    * [[ScoreCriteria.fromContent]] for structured ones. */
  final class ScoreCriteria private (val levels: List[Content | Null]):
    def size: Int = levels.size

  object ScoreCriteria:
    val MinLevels = 2
    val MaxLevels = 10

    def apply(levels: (String | Null)*): Either[String, ScoreCriteria] =
      fromContent(levels.map {
        case null => null
        case s    => Content(s)
      }.toList)

    def fromContent(levels: List[Content | Null]): Either[String, ScoreCriteria] =
      if levels.size < MinLevels || levels.size > MaxLevels then
        Left(s"Score.criteria needs between $MinLevels and $MaxLevels levels, got ${levels.size}")
      else
        Right(new ScoreCriteria(levels))

  /** One typed question asked of a piece of `state`. The three cases
    * are exhaustive — the decision model has no other question shape.
    *
    * Generic in `S`, the type of `instructions` — a real type parameter,
    * not erased to [[Content]]: `.instructions` reads back exactly the
    * value you built the question with, no decode step, because nothing
    * downstream needs it erased. The wire only needs *some* uniform
    * representation for instructions once several differently-typed
    * questions are combined in one [[ask]] call, and `Helpers` produces
    * that (a `zio.json.ast.Json` node, via the `schema` field below)
    * only at the point of actually building the request — never as part
    * of this type's own public shape.
    *
    * Deliberately a `sealed trait` + plain classes rather than a Scala
    * `enum` or `case class`:
    *
    *   - An `enum` case's inferred type is widened to the enum type
    *     itself wherever it isn't otherwise constrained (e.g. a bare
    *     `NamedTuple` field) — exactly the position [[ask]] infers
    *     `Values` from — which would collapse every question to plain
    *     `Question[?]` and make [[AnswerOf]] unable to pick a branch. A
    *     plain class carries its own precise type, so
    *     `ask(..., (isUrgent = Question.Noul("...")))` infers that
    *     field as `Question.Noul[String]`, not `Question[String]` or
    *     `Question[?]`.
    *   - The public constructor is `apply[S: Schema](instructions: S, ...)`
    *     — a single required argument, so `S` always infers cleanly
    *     from whatever's passed (a `String`, or any `Schema`-derived
    *     record) with an ordinary, visible type-class bound. That needs
    *     a hand-written companion rather than the one a `case class`
    *     synthesizes, since the captured `Schema[S]` has to be stored
    *     alongside `instructions` for `Helpers` to use later.
    *
    * The `NamedTuple` key a `Question` is registered under in [[ask]]
    * becomes both the wire question id and the field name of the
    * matching typed answer in the result — see [[AnswerOf]]. */
  sealed trait Question[S]
  object Question:
    final class Noul[S] private[zio_decisions] (
      val instructions: S,
      val criteria:      NoulCriteria | Null,
      private[zio_decisions] val schema: Schema[S],
    ) extends Question[S]
    object Noul:
      def apply[S: Schema](instructions: S, criteria: NoulCriteria | Null = null): Noul[S] =
        new Noul[S](instructions, criteria, summon[Schema[S]])

    final class Choice[S] private[zio_decisions] (
      val instructions: S,
      val criteria:      ChoiceCriteria,
      private[zio_decisions] val schema: Schema[S],
    ) extends Question[S]
    object Choice:
      def apply[S: Schema](instructions: S, criteria: ChoiceCriteria): Choice[S] =
        new Choice[S](instructions, criteria, summon[Schema[S]])

    final class Score[S] private[zio_decisions] (
      val instructions: S,
      val criteria:      ScoreCriteria,
      private[zio_decisions] val schema: Schema[S],
    ) extends Question[S]
    object Score:
      def apply[S: Schema](instructions: S, criteria: ScoreCriteria): Score[S] =
        new Score[S](instructions, criteria, summon[Schema[S]])

  // ---------- Answers ----------

  /** [[Question.Choice]]'s answer: the selected option, the full
    * probability distribution over every registered option (sums to
    * ~1.0), and the derived [[confidence]]. */
  case class ChoiceAnswer(
    choice:        String,
    probabilities: Map[String, Probability],
    confidence:    Probability,
  )

  /** [[Question.Score]]'s answer: the probability-weighted mean over the
    * registered levels (`Σ level × P(level)`, `0`-indexed, so it can be
    * fractional), the per-level probabilities and legend, and the
    * derived [[confidence]]. */
  case class ScoreAnswer(
    score:         Double,
    legend:        Map[Int, String],
    probabilities: Map[Int, Probability],
    confidence:    Probability,
  )

  /** Runtime answer shape for [[askDynamic]]. Unlike [[AnswerOf]], which
    * preserves a compile-time NamedTuple's exact field types, this ADT keeps
    * the three answer variants explicit in a runtime-sized map. */
  enum DynamicAnswer:
    case Noul(probability: Probability)
    case Choice(answer: ChoiceAnswer)
    case Score(answer: ScoreAnswer)

  /** Per-question answer type, selected by the registered [[Question]]
    * subtype. A `Noul` question answers with a bare [[Probability]] —
    * no wrapper needed, since "the probability the answer is yes" is
    * its entire answer. */
  type AnswerOf[Q] <: Matchable = Q match
    case Question.Noul[?]   => Probability
    case Question.Choice[?] => ChoiceAnswer
    case Question.Score[?]  => ScoreAnswer

  /** Per-tuple-element answer types, in order — the payload half of the
    * `NamedTuple` `.answers` / `.run` return. */
  type AnswersOf[Hs <: Tuple] <: Tuple = Hs match
    case h *: rest  => AnswerOf[h] *: AnswersOf[rest]
    case EmptyTuple => EmptyTuple

  /** Compile-time witness that every element of `Hs` is a [[Question]].
    * Built inductively — as `zio-bedrock-converse`'s `AllTools` is — so
    * a stray non-`Question` element is named in the error rather than
    * producing an opaque match-type failure. */
  sealed trait AllQuestions[Hs <: Tuple]
  object AllQuestions:
    given empty: AllQuestions[EmptyTuple] = new AllQuestions[EmptyTuple] {}

    given consNoul[S, Tail <: Tuple](using AllQuestions[Tail]): AllQuestions[Question.Noul[S] *: Tail] =
      new AllQuestions[Question.Noul[S] *: Tail] {}
    given consChoice[S, Tail <: Tuple](using AllQuestions[Tail]): AllQuestions[Question.Choice[S] *: Tail] =
      new AllQuestions[Question.Choice[S] *: Tail] {}
    given consScore[S, Tail <: Tuple](using AllQuestions[Tail]): AllQuestions[Question.Score[S] *: Tail] =
      new AllQuestions[Question.Score[S] *: Tail] {}

  // ---------- Usage / Result ----------

  case class Usage(inputTokens: Int, outputTokens: Int)

  /** Full response envelope. `answers` is exactly the shape [[ask]]'s
    * `.answers` terminal returns on its own — a `NamedTuple` keyed like
    * the registered questions. */
  case class Result[+T](answers: T, model: ModelId, usage: Usage)

  /** Outcome of one physical [[DecisionModel.decide]] exchange. A successful
    * response is the complete decoded response re-encoded as canonical JSON;
    * it is not the byte-exact HTTP response body. */
  enum ExchangeOutcome:
    case Success(response: Json)
    case Failure(cause: Cause[Error])

  /** Complete JSON request, exchange outcome, and elapsed wall-clock time.
    * Deliberately contains no HTTP headers or bearer tokens. Bodies may still
    * contain sensitive application state and model output. */
  case class ExchangeObservation(request: Json, outcome: ExchangeOutcome, latencyMs: Long)

  /** Best-effort observer of every physical decision-model exchange. Requests
    * and responses are the provider-neutral canonical JSON (see [[Middleware]]). */
  trait ExchangeObserver:
    def observe(observation: ExchangeObservation): UIO[Unit]

  object ExchangeObserver:
    val none: ExchangeObserver = fromFunction(_ => ZIO.unit)

    def fromFunction(f: ExchangeObservation => UIO[Unit]): ExchangeObserver =
      new ExchangeObserver:
        def observe(observation: ExchangeObservation): UIO[Unit] = f(observation)

    /** Logs complete request and canonical response bodies. Enable only where
      * logs are approved to contain the state and answer payloads. */
    val logging: ExchangeObserver = fromFunction: observation =>
      observation.outcome match
        case ExchangeOutcome.Success(response) =>
          ZIO.logInfo(
            s"Decision exchange completed in ${observation.latencyMs}ms; request=${observation.request.toJson}; response=${response.toJson}"
          )
        case ExchangeOutcome.Failure(cause) =>
          ZIO.logErrorCause(
            s"Decision exchange failed in ${observation.latencyMs}ms; request=${observation.request.toJson}",
            cause,
          )

  /** Run a callback without allowing its defects or self-interruption to alter
    * client/loop semantics. Observer failures are reported through ZIO's
    * logger; a logger defect is itself suppressed as a last resort. */
  private[zio_decisions] def notifyBestEffort(label: String)(effect: => UIO[Any]): UIO[Unit] =
    ZIO.suspendSucceed(effect).unit
      .catchAllCause(cause =>
        ZIO.logErrorCause(s"$label observer failed", cause)
          .catchAllCause(_ => ZIO.unit)
      )
      .uninterruptible

  /** One host-generated action the decision model may select during a loop iteration. */
  case class LoopOption[+A](id: String, value: A, description: Content)

  object LoopOption:
    def text[A](id: String, value: A, description: String): LoopOption[A] =
      LoopOption(id, value, Content(description))

  /** Result of handling one model-selected action. */
  enum LoopStep[S, +O]:
    case Continue(state: S)
    case Done(output: O)

  /** Decision metadata for one decision-loop iteration. */
  case class LoopTurn(
    iteration: Int,
    choice: String,
    answer: ChoiceAnswer,
    usage: Usage,
    latencyMs: Long,
  )

  /** Final loop output, every decision turn, aggregate decision usage, and
    * aggregate wall-clock time spent in decision requests. */
  case class LoopResult[+O](output: O, turns: List[LoopTurn], usage: Usage, latencyMs: Long)

  /** Retry configuration for each individual decision request. The first
    * retry waits `initialDelay`, with exponential backoff thereafter. */
  case class LoopRetryPolicy(maxRetries: Int, initialDelay: Duration = 500.millis)

  /** Serializable option metadata retained by loop observations and audits.
    * The host action value is intentionally excluded. */
  case class LoopAuditOption(id: String, description: Content)

  /** State view and legal option metadata presented for one successful
    * decision. */
  case class LoopAuditTurn(iteration: Int, stateView: Content, options: List[LoopAuditOption])

  /** Opt-in state/option audit paired with the unchanged [[LoopResult]]. */
  case class AuditedLoopResult[+O](result: LoopResult[O], audit: List[LoopAuditTurn]):
    def output: O = result.output
    def turns: List[LoopTurn] = result.turns
    def usage: Usage = result.usage
    def latencyMs: Long = result.latencyMs

  /** Semantic lifecycle events for one logical loop execution. Retry attempts
    * do not generate additional options or decision events. */
  enum LoopObservation[+E, +O]:
    case OptionsGenerated(iteration: Int, stateView: Content, options: List[LoopAuditOption])
    case Decision(turn: LoopTurn)
    case Completion(result: LoopResult[O])
    case Failure(cause: Cause[Error | E], turns: List[LoopTurn])

  /** Best-effort semantic observer for a loop with host error `E` and output
    * `O`. Callback defects and self-interruption are isolated. */
  trait LoopObserver[E, O]:
    def observe(observation: LoopObservation[E, O]): UIO[Unit]

  object LoopObserver:
    def none[E, O]: LoopObserver[E, O] = make(_ => ZIO.unit)

    def make[E, O](f: LoopObservation[E, O] => UIO[Unit]): LoopObserver[E, O] =
      new LoopObserver[E, O]:
        def observe(observation: LoopObservation[E, O]): UIO[Unit] = f(observation)

  // ---------- Errors ----------

  /** Every failure a decision model or this library's request builders can
    * produce, whichever provider answered. HTTP providers map their status
    * codes onto the same cases, so callers (and the orchestration) act on one
    * error type. */
  sealed trait Error extends Throwable:
    def errorMessage: String
    override def getMessage: String = errorMessage

  object Error:
    final case class BadRequest         (message: String)                 extends Error:
      def errorMessage = s"400 BadRequest: $message"
    final case class Authentication     (message: String)                 extends Error:
      def errorMessage = s"401 Authentication: $message"
    final case class PermissionDenied   (message: String)                 extends Error:
      def errorMessage = s"403 PermissionDenied: $message"
    final case class NotFound           (message: String)                 extends Error:
      def errorMessage = s"404 NotFound: $message"
    final case class UnprocessableEntity(message: String)                 extends Error:
      def errorMessage = s"422 UnprocessableEntity: $message"
    final case class RateLimit          (message: String)                 extends Error:
      def errorMessage = s"429 RateLimit: $message"
    final case class ServiceOverloaded  (message: String)                 extends Error:
      def errorMessage = s"529 ServiceOverloaded: $message"
    final case class InternalServer     (status: Status, message: String) extends Error:
      def errorMessage = s"${status.code} InternalServer: $message"
    final case class Unexpected         (status: Status, body: String)    extends Error:
      def errorMessage = s"unexpected ${status.code}: $body"
    final case class Transport          (cause: Throwable)                extends Error:
      def errorMessage = s"Transport error: ${cause.getMessage}"
    /** The provider's API key isn't configured: `variable` names the
      * environment variable its `live` layer reads. */
    final case class MissingApiKey      (variable: String)                extends Error:
      def errorMessage = s"Missing API key (set $variable)"
    /** The request was more than the model takes (its context length); the
      * orchestration asks again, smaller. */
    final case class TooLong            (message: String)                 extends Error:
      def errorMessage = s"Request too long: $message"
    /** A response that decoded fine as JSON but violated the wire
      * contract — e.g. a `noul` / `confidence` / probability outside
      * `[0, 1]`, or an answer whose `type` doesn't match the question
      * that was asked. */
    final case class MalformedAnswer    (questionId: QuestionId, message: String) extends Error:
      def errorMessage = s"Malformed answer for question '$questionId': $message"
    /** The response omitted an answer for a question that was asked. */
    final case class MissingAnswer      (questionId: QuestionId)          extends Error:
      def errorMessage = s"No answer returned for question '$questionId'"
    /** The model declined to answer a question (OpenAI's `refusal`). */
    final case class Refused            (questionId: QuestionId)          extends Error:
      def errorMessage = s"The model refused question '$questionId'"
    final case class InvalidLoop       (message: String)                  extends Error:
      def errorMessage = s"Invalid decision loop: $message"
    final case class MaxIterations     (iterations: Int)                  extends Error:
      def errorMessage = s"Decision loop exceeded maxIterations = $iterations"

    /** A failure worth retrying as it is: the connection, throttling, an
      * overloaded or failing service. */
    def isTransient(error: Error): Boolean = error match
      case _: (Transport | RateLimit | ServiceOverloaded | InternalServer) => true
      case _                                                              => false

    private[zio_decisions] def fromStatus(status: Status, body: String): Error =
      status.code match
        case 400                     => BadRequest(body)
        case 401                     => Authentication(body)
        case 403                     => PermissionDenied(body)
        case 404                     => NotFound(body)
        case 422                     => UnprocessableEntity(body)
        case 429                     => RateLimit(body)
        case 529                     => ServiceOverloaded(body)
        case c if c >= 500 && c < 600 => InternalServer(status, body)
        case _                        => Unexpected(status, body)

    extension [R, A](zio: ZIO[R, Error, A])
      /** Retries `RateLimit` / `ServiceOverloaded` / `InternalServer` up
        * to 2x with exponential backoff: back off rather than retry
        * immediately. */
      def retryOnRetryable: ZIO[R, Error, A] =
        zio.retry(
          Schedule.recurWhile[Error]:
            case _: (RateLimit | ServiceOverloaded | InternalServer) => true
            case _                                                   => false
          && Schedule.exponential(500.millis)
          && Schedule.recurs(2)
        )

  // ---------- Request builder ----------

  /** Pure-data builder returned by [[ask]]. `.answers` / `.run` pull a
    * [[DecisionModel]] from the ZIO env to make the round-trip.
    *
    * Parameterized directly over a `NamedTuple`'s `Names` and `Values`
    * rather than a single `NT <: NamedTuple.AnyNamedTuple`: with both in
    * hand, `questions.toTuple` (the stdlib's own, compiler-checked
    * unwrap of a `NamedTuple` to its underlying `Values` tuple) resolves
    * directly wherever `NT` is used — an ordinary generic method can't
    * decompose a single opaque `NT` that way, only reach it via the
    * `NamedTuple.Names[NT]` / `NamedTuple.DropNames[NT]` match-type
    * projections, which don't carry enough structure for the extension
    * method to apply and would force falling back to an unchecked
    * `questions.asInstanceOf[Tuple]`. */
  final class AskRequest[Names <: Tuple, Values <: Tuple] @scala.annotation.publicInBinary private[zio_decisions] (
    private[zio_decisions] val state:   Content,
    // `Option`, not `ModelId | Null`: this field is a purely internal
    // "did the caller override the model" sentinel that no user ever
    // sees or sets directly (the public surface is `.model(m: ModelId)`,
    // always a real value) — so there's no ergonomic reason to fight
    // opaque-type-vs-null friction here the way the public `T | Null`
    // convention is worth it for a field users actually construct.
    private[zio_decisions] val model:   Option[ModelId],
    private[zio_decisions] val entries: List[(QuestionId, Question[?])],
  ):
    /** Override the model for this request; defaults to the [[DecisionModel]]'s
      * configured model. */
    def model(m: ModelId): AskRequest[Names, Values] =
      new AskRequest[Names, Values](state, Some(m), entries)

    /** Just the typed answers, keyed exactly like the registered
      * questions. */
    def answers: ZIO[DecisionModel, Error, NamedTuple.NamedTuple[Names, AnswersOf[Values]]] =
      run.map(_.answers)

    /** Full envelope: typed answers plus the responding model and token
      * usage. `Names` / `Values` propagate into `RequestImpl.run`, which
      * returns this exact type — no cast needed here at the public
      * boundary; the one truly unavoidable cast (bridging our own
      * runtime-verified-but-type-checker-opaque decode of each
      * question's answer) lives entirely inside `RequestImpl`. */
    def run: ZIO[DecisionModel, Error, Result[NamedTuple.NamedTuple[Names, AnswersOf[Values]]]] =
      RequestImpl.run(this)


  /** Pure-data builder returned by [[askDynamic]]. Questions may be assembled
    * at runtime; answers therefore return as a map of explicit
    * [[DynamicAnswer]] variants rather than a compile-time NamedTuple. */
  final class DynamicAskRequest private[zio_decisions] (
    private[zio_decisions] val state: Content,
    private[zio_decisions] val model: Option[ModelId],
    private[zio_decisions] val entries: List[(QuestionId, Question[?])],
  ):
    /** Override the model for this request; defaults to the [[DecisionModel]]'s. */
    def model(m: ModelId): DynamicAskRequest =
      new DynamicAskRequest(state, Some(m), entries)

    /** Runtime-sized answers keyed by the supplied question ids. */
    def answers: ZIO[DecisionModel, Error, Map[QuestionId, DynamicAnswer]] =
      run.map(_.answers)

    /** Full response envelope including resolved model and token usage. */
    def run: ZIO[DecisionModel, Error, Result[Map[QuestionId, DynamicAnswer]]] =
      RequestImpl.runDynamic(this)
  private val DefaultLoopChoiceInstructions = Content(
    "Choose the single action that best advances the current state toward completion. Option ids are opaque; judge their structured descriptions."
  )

  /** Configurable Decision-model-driven state-machine loop. Host code renders state,
    * generates valid actions, and handles the selected action as Continue or
    * Done. Every copy-style method preserves all other configuration. */
  final class LoopRequest[S, A, R, E, O] private[zio_decisions] (
    private[zio_decisions] val initial: S,
    private[zio_decisions] val stateView: S => Content,
    private[zio_decisions] val options: S => ZIO[R, E, NonEmptyChunk[LoopOption[A]]],
    private[zio_decisions] val handler: (S, A, LoopTurn) => ZIO[R, E, LoopStep[S, O]],
    private[zio_decisions] val model: Option[ModelId],
    private[zio_decisions] val maxIter: Int,
    private[zio_decisions] val instructions: Content,
    private[zio_decisions] val retryPolicy: LoopRetryPolicy,
    private[zio_decisions] val observer: LoopObserver[E, O],
  ):
    private def copy(
      model: Option[ModelId] = model,
      maxIter: Int = maxIter,
      instructions: Content = instructions,
      retryPolicy: LoopRetryPolicy = retryPolicy,
      observer: LoopObserver[E, O] = observer,
    ): LoopRequest[S, A, R, E, O] =
      new LoopRequest(initial, stateView, options, handler, model, maxIter, instructions, retryPolicy, observer)

    def model(value: ModelId): LoopRequest[S, A, R, E, O] =
      copy(model = Some(value))

    def maxIterations(value: Int): LoopRequest[S, A, R, E, O] =
      copy(maxIter = value)

    /** Replaces the default Choice instructions. Structured `Schema` values
      * remain objects/arrays/null on the wire rather than being stringified. */
    def choiceInstructions[I: Schema](value: I): LoopRequest[S, A, R, E, O] =
      copy(instructions = Content(value))

    /** Retries only retryable failures from each per-turn decision request. */
    def retryEachTurn(policy: LoopRetryPolicy): LoopRequest[S, A, R, E, O] =
      copy(retryPolicy = policy)

    def observe(value: LoopObserver[E, O]): LoopRequest[S, A, R, E, O] =
      copy(observer = value)

    def run: ZIO[DecisionModel & R, Error | E, LoopResult[O]] = LoopImpl.run(this)

    /** Runs with an additional Content-only snapshot of each successfully
      * selected turn. Ordinary `run` does not return or retain this audit. */
    def runAudited: ZIO[DecisionModel & R, Error | E, AuditedLoopResult[O]] = LoopImpl.runAudited(this)

  /** Builds a decision loop whose handler needs only the selected host action.
    * Use [[loopWithTurn]] when the transition must inspect the decision model's full
    * probability distribution or other per-turn metadata. */
  def loop[S, A, R, E, O](initial: S)(
    stateView: S => Content,
    options: S => ZIO[R, E, NonEmptyChunk[LoopOption[A]]],
  )(
    handler: (S, A) => ZIO[R, E, LoopStep[S, O]],
  ): LoopRequest[S, A, R, E, O] =
    new LoopRequest(
      initial,
      stateView,
      options,
      (state, action, _) => handler(state, action),
      None,
      10,
      DefaultLoopChoiceInstructions,
      LoopRetryPolicy(0),
      LoopObserver.none,
    )

  /** Builds a decision loop whose handler receives the completed [[LoopTurn]] for
    * the selected action. The turn contains the full [[ChoiceAnswer]], so a
    * host can retain several likely branches, confidence-gate a transition,
    * or otherwise use the calibrated distribution before constructing the
    * next state. */
  def loopWithTurn[S, A, R, E, O](initial: S)(
    stateView: S => Content,
    options: S => ZIO[R, E, NonEmptyChunk[LoopOption[A]]],
  )(
    handler: (S, A, LoopTurn) => ZIO[R, E, LoopStep[S, O]],
  ): LoopRequest[S, A, R, E, O] =
    new LoopRequest(
      initial,
      stateView,
      options,
      handler,
      None,
      10,
      DefaultLoopChoiceInstructions,
      LoopRetryPolicy(0),
      LoopObserver.none,
    )

  /** Ask the decision model one or more questions about `state` in a single round-trip.
    * The `NamedTuple`'s keys become both the wire question ids and the
    * field names of the `.answers` / `.run` result. `state` is generic
    * in a `Schema` type parameter — pass a plain `String`, or any
    * `Schema`-derived record, taxonomy, or database row directly; `S` is
    * inferred from whatever you pass, the same way each [[Question]]
    * factory infers its `instructions`.
    *
    * Compile-time enforcement:
    *   - The tuple is non-empty.
    *   - Every element is a [[Question]].
    *
    * {{{
    * Decisions.ask(
    *   "Hi, I've been trying to connect my Stripe account for 3 days...",
    *   (
    *     isUrgent   = Question.Noul("Does this message express urgency?"),
    *     department = Question.Choice("Which team should handle this?", deptCriteria),
    *   ),
    * ).answers
    * // : ZIO[DecisionModel, Error, (isUrgent: Probability, department: ChoiceAnswer)]
    * }}} */
  inline def ask[S: Schema, Names <: Tuple, Values <: Tuple](
    state:     S,
    questions: NamedTuple.NamedTuple[Names, Values],
  )(using
    inline ev:  Values <:< NonEmptyTuple,
    inline all: AllQuestions[Values],
  ): AskRequest[Names, Values] =
    // `constValueTuple[Names].toList` and `questions.toTuple.toList` are
    // each, individually, a `Tuple` known (only) to be homogeneous in
    // `String` / `Question[?]` by the `using` evidence above — a fact
    // the match-type reducer behind `Tuple#toList` can't consult for an
    // abstract `Names` / `Values`, so it can't narrow past
    // `List[Tuple.Union[_]]` on its own. The casts below are exactly
    // that evidence-to-type-checker bridge, nothing more.
    val names:   List[String]      = compiletime.constValueTuple[Names].toList.asInstanceOf[List[String]]
    val values:  List[Question[?]] = questions.toTuple.toList.asInstanceOf[List[Question[?]]]
    val entries = names.iterator.zip(values.iterator).map((n, q) => (QuestionId(n), q)).toList
    new AskRequest[Names, Values](Content(state), None, entries)


  /** Ask a runtime-sized set of questions about one shared state in a single
    * decision round-trip. This complements [[ask]]: use `ask` when question names
    * and answer types are known at compile time, and `askDynamic` for catalogs,
    * candidates, or fan-out sets discovered at runtime.
    *
    * The iterable must be non-empty and question ids must be unique. The
    * validated request preserves entry order on the wire; answers are keyed by
    * [[QuestionId]] and retain their answer variant in [[DynamicAnswer]]. */
  def askDynamic[S: Schema](
    state: S,
    questions: Iterable[(QuestionId, Question[?])],
  ): Either[String, DynamicAskRequest] =
    val entries = questions.iterator.toList
    val duplicateIds = entries.groupMapReduce(_._1)(_ => 1)(_ + _).collect:
      case (questionId, count) if count > 1 => questionId
    if entries.isEmpty then Left("askDynamic requires at least one question")
    else if duplicateIds.nonEmpty then
      Left(s"askDynamic question ids must be unique; duplicates: ${duplicateIds.toList.sorted.mkString(", ")}")
    else Right(new DynamicAskRequest(Content(state), None, entries))
