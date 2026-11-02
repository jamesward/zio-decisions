package com.jamesward.zio_decisions.openai

import com.jamesward.zio_decisions.DecisionModel
import com.jamesward.zio_decisions.Decisions.*
import com.jamesward.zio_decisions.internal.Canonical
import zio.*
import zio.http.{Body, Client, Header, MediaType, Status, URL}
import zio.json.*
import zio.json.ast.Json

/**
 * OpenAI's Decisions API (`POST https://api.openai.com/v1/decisions`,
 * https://developers.openai.com/api/docs/guides/decisions) as a [[DecisionModel]]. The library's questions map onto
 * OpenAI's named questions: `Noul` is a `predicate` (its true/false criteria appended to its instructions), `Choice` a
 * `choice` (an option's description its `description`, or its value when it has none), `Score` a `score` over its levels
 * (labeled `Level 0`, `Level 1`, …, each described by its criteria). The state is the shared `input`: a string as itself,
 * any other value as its JSON. A refused question fails the decision with [[Error.Refused]].
 *
 * {{{
 * program.provide(Client.default, OpenAI.live)
 * program.provide(Client.default, OpenAI.layer(OpenAI.ApiKey("sk-…"), OpenAI.DefaultModel))
 * }}}
 */
object OpenAI:

  val Provider = "openai"

  /** An OpenAI API key (`OPENAI_API_KEY`). */
  opaque type ApiKey = String
  object ApiKey:
    def apply(value: String): ApiKey = value
    extension (key: ApiKey) def unwrap: String = key

  /** The model the Decisions API serves; `gpt-6-luna` is the only one today. */
  val DefaultModel: ModelId = ModelId("gpt-6-luna")

  val DefaultEndpoint: URL = URL.decode("https://api.openai.com").toOption.get
  val Path = "/v1/decisions"

  /** What a 400's body says when the request was more than the model takes. */
  private val TooLongMarkers = List("context_length_exceeded", "too long", "maximum context")

  /** OpenAI's Decisions API as a decision model. */
  final class Model private[zio_decisions] (apiKey: ApiKey, val defaultModel: ModelId, client: Client, endpoint: URL)
      extends DecisionModel:
    val provider: String = Provider

    private val authed: Client = client
      .url(endpoint)
      .addHeader(Header.Authorization.Bearer(apiKey: String))
      .addHeader(Header.ContentType(MediaType.application.json))

    def decide(request: DecisionModel.Request): IO[Error, DecisionModel.Decision] =
      val model = request.model.getOrElse(defaultModel)
      val body = Wire.Request(model.unwrap, text(request.state.json), request.questions.map(question))
      for
        reply    <- ZIO.scoped(authed.post(Path)(Body.fromString(body.toJson)).flatMap(r => r.body.asString.map(r.status -> _)))
                      .mapError(Error.Transport.apply)
        response <- parse(reply._1, reply._2)
        answers  <- ZIO.foreach(request.questions): (id, q) =>
                      ZIO.fromEither(answer(id, q, response.answers.find(_.name == id.unwrap)).map(id -> _))
      yield DecisionModel.Decision(answers.toMap, response.model.fold(model)(ModelId(_)), usage(response.usage))

  /** OpenAI's Decisions API, from an explicit API key and model, at `endpoint` (OpenAI's API unless given: a proxy, or
    * a test double speaking the same wire protocol). */
  def layer(apiKey: ApiKey, model: ModelId = DefaultModel, endpoint: URL = DefaultEndpoint): URLayer[Client, DecisionModel] =
    ZLayer.fromFunction((client: Client) => new Model(apiKey, model, client, endpoint))

  /** Reads `OPENAI_API_KEY` (required) and `OPENAI_DECISIONS_MODEL` (defaults to `gpt-6-luna`). */
  val live: ZLayer[Client, Error, DecisionModel] =
    ZLayer.fromZIO:
      for
        apiKey <- System.env("OPENAI_API_KEY").orDie.map(_.map(_.trim).filter(_.nonEmpty))
                    .someOrFail(Error.MissingApiKey("OPENAI_API_KEY"))
        model  <- System.env("OPENAI_DECISIONS_MODEL").orDie.map(_.map(_.trim).filter(_.nonEmpty).fold(DefaultModel)(ModelId(_)))
        client <- ZIO.service[Client]
      yield new Model(ApiKey(apiKey), model, client, DefaultEndpoint)

  /** A score level's label: OpenAI's levels are labeled; the library's are only ordered and described. */
  def label(index: Int): String = s"Level $index"

  /** One of the library's questions as OpenAI's, named by its id. */
  def question(entry: (QuestionId, Question[?])): Wire.Question =
    val (id, q) = entry
    val instructions = text(Canonical.instructions(q))
    q match
      case n: Question.Noul[?] =>
        val criteria = n.criteria match
          case null            => ""
          case c: NoulCriteria =>
            described(c.whenTrue).fold("")(t => s"\nTrue when: $t") + described(c.whenFalse).fold("")(f => s"\nFalse when: $f")
        Wire.Question.Predicate(id.unwrap, instructions + criteria)
      case c: Question.Choice[?] =>
        Wire.Question.Choice(id.unwrap, instructions,
          c.criteria.options.toList.map((value, description) => Wire.ChoiceOption(value, described(description).getOrElse(value))))
      case sc: Question.Score[?] =>
        Wire.Question.Score(id.unwrap, instructions,
          sc.criteria.levels.zipWithIndex.map((level, i) => Wire.Level(label(i), described(level).getOrElse(label(i)))))

  /** OpenAI's answer to a question as the library's, when it is one of the question's kind. */
  def answer(id: QuestionId, q: Question[?], a: Option[Wire.Answer]): Either[Error, DynamicAnswer] =
    def malformed(message: String) = Error.MalformedAnswer(id, message)
    (q, a) match
      case (_, None) =>
        Left(Error.MissingAnswer(id))
      case (_, Some(Wire.Answer.Refusal(_))) =>
        Left(Error.Refused(id))
      case (_: Question.Noul[?], Some(Wire.Answer.Predicate(_, p))) =>
        Probability(p).map(DynamicAnswer.Noul(_)).left.map(malformed)
      case (_: Question.Choice[?], Some(Wire.Answer.Choice(_, choice, probs, conf))) =>
        (for
          ps <- probabilities(probs.map(p => p.value -> p.probability))
          c  <- Probability(conf)
        yield DynamicAnswer.Choice(ChoiceAnswer(choice, ps, c))).left.map(malformed)
      case (sc: Question.Score[?], Some(Wire.Answer.Score(_, score, probs, conf))) =>
        val legend = sc.criteria.levels.zipWithIndex.map((level, i) => i -> described(level).getOrElse(label(i))).toMap
        (for
          ps <- probabilities(probs.map(p => p.value -> p.probability))
          c  <- Probability(conf)
        yield DynamicAnswer.Score(ScoreAnswer(score, legend, ps, c))).left.map(malformed)
      case (_, Some(other)) =>
        Left(malformed(s"answer type doesn't match the question that was asked: $other"))

  /** An HTTP error as the library's: a 400 saying the request was too long is [[Error.TooLong]]. */
  def failure(status: Status, body: String): Error =
    if status.code == 400 && TooLongMarkers.exists(body.contains) then Error.TooLong(body)
    else Error.fromStatus(status, body)

  /** A response's body as its answers, when the status is a success; else the status's failure. */
  def parse(status: Status, body: String): IO[Error, Wire.Response] =
    if status.isSuccess then
      ZIO.fromEither(body.fromJson[Wire.Response]).mapError(e => Error.Unexpected(status, s"Decode failed: $e"))
    else ZIO.fail(failure(status, body))

  private def usage(usage: Option[Wire.Usage]): Usage =
    usage.fold(Usage(0, 0))(u => Usage(u.inputTokens.getOrElse(0), u.outputTokens.getOrElse(0)))

  private def probabilities[K](entries: List[(K, Double)]): Either[String, Map[K, Probability]] =
    entries.foldLeft[Either[String, Map[K, Probability]]](Right(Map.empty)):
      case (acc, (k, p)) => acc.flatMap(m => Probability(p).map(m.updated(k, _)))

  private def described(content: Content | Null): Option[String] = content match
    case null       => None
    case c: Content => Some(text(c.json))

  /** JSON as the text OpenAI reads: a string as itself, anything else as JSON. */
  private def text(json: Json): String = json match
    case Json.Str(s) => s
    case other       => other.toJson

  /**
   * The Decisions API's wire types: a request is a model, the shared input and named typed questions (predicate,
   * choice, score); the response answers each question by its name, or refuses it.
   */
  object Wire:

    /** One of a choice question's options: its value, and when it applies. */
    final case class ChoiceOption(value: String, description: String) derives JsonEncoder

    /** One of a score question's levels, ordered lowest first: its label, and its criteria. */
    final case class Level(label: String, description: String) derives JsonEncoder

    /** A question about the input, named uniquely in its request. */
    @jsonDiscriminator("type")
    sealed trait Question derives JsonEncoder:
      def name: String

    object Question:
      /** The probability that a condition is true. */
      @jsonHint("predicate")
      final case class Predicate(name: String, instructions: String) extends Question

      /** One of the options, with a probability for each. */
      @jsonHint("choice")
      final case class Choice(name: String, instructions: String, choices: List[ChoiceOption]) extends Question

      /** A score over ordered levels: the probability-weighted average of their indices (from 0). */
      @jsonHint("score")
      final case class Score(name: String, instructions: String, levels: List[Level]) extends Question

    /** A request: the shared input (text) and the questions about it. */
    final case class Request(model: String, input: String, questions: List[Question]) derives JsonEncoder

    /** A choice answer's probability for one option. */
    final case class ChoiceProbability(value: String, probability: Double) derives JsonDecoder

    /** A score answer's probability for one level, by its index. */
    final case class LevelProbability(value: Int, label: Option[String], probability: Double) derives JsonDecoder

    /** An answer, to the question of its name. */
    @jsonDiscriminator("type")
    sealed trait Answer derives JsonDecoder:
      def name: String

    object Answer:
      @jsonHint("predicate")
      final case class Predicate(name: String, probability: Double) extends Answer

      @jsonHint("choice")
      final case class Choice(name: String, choice: String, probabilities: List[ChoiceProbability], confidence: Double)
          extends Answer

      @jsonHint("score")
      final case class Score(name: String, score: Double, probabilities: List[LevelProbability], confidence: Double)
          extends Answer

      /** The model declined to answer the question. */
      @jsonHint("refusal")
      final case class Refusal(name: String) extends Answer

    /** The tokens a request used, when the response says. */
    final case class Usage(@jsonField("input_tokens") inputTokens: Option[Int] = None,
        @jsonField("output_tokens") outputTokens: Option[Int] = None) derives JsonDecoder

    /** A response: an answer for each question, the model that answered, the usage. */
    final case class Response(answers: List[Answer], model: Option[String] = None, usage: Option[Usage] = None)
        derives JsonDecoder
