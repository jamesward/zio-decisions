package com.jamesward.zio_decisions.typesafe

import com.jamesward.zio_decisions.DecisionModel
import com.jamesward.zio_decisions.Decisions.*
import com.jamesward.zio_decisions.internal.{Canonical, Codecs}
import zio.*
import zio.http.{Body, Client, Header, MediaType, Status, URL}
import zio.json.*
import zio.schema.codec.JsonCodec

import java.nio.charset.StandardCharsets.UTF_8

/**
 * TypeSafe AI's Jev / System One API (`POST https://api.typesafe.ai/v1/systemone`, docs.typesafe.ai) as a
 * [[DecisionModel]]. System One speaks this library's question language natively: the request is the canonical JSON
 * as is, and every answer comes back keyed by its question id.
 *
 * {{{
 * program.provide(Client.default, TypeSafe.live)
 * program.provide(Client.default, TypeSafe.layer(TypeSafe.ApiKey("…"), TypeSafe.JevLatest))
 * }}}
 */
object TypeSafe:

  val Provider = "typesafe"

  opaque type ApiKey = String
  object ApiKey:
    def apply(s: String): ApiKey = s
    extension (k: ApiKey) def unwrap: String = k

  /** The documented default model. */
  val JevLatest: ModelId = ModelId("jev-latest")

  val DefaultEndpoint: URL = URL.decode("https://api.typesafe.ai").toOption.get
  val Path = "/v1/systemone"

  /** What a 400's body says when the request was more than Jev takes. */
  private val TooLongMarker = "max_tokens_exceeded"

  /** One System One exchange, below the decision model: the live HTTP transport, or a test double. */
  private[zio_decisions] trait Transport:
    def send(request: Wire.SystemOneRequest): IO[Error, Wire.SystemOneResponse]

  /** Jev as a decision model, over `transport`. */
  final class Model private[zio_decisions] (transport: Transport, val defaultModel: ModelId) extends DecisionModel:
    val provider: String = Provider

    def decide(request: DecisionModel.Request): IO[Error, DecisionModel.Decision] =
      val model = request.model.getOrElse(defaultModel)
      transport.send(Wire.SystemOneRequest(Canonical.request(request.state, model.unwrap, request.questions)))
        .mapError(tooLong)
        .flatMap: wire =>
          ZIO.fromEither(answers(request.questions, wire)).map: answers =>
            DecisionModel.Decision(answers, ModelId(wire.model), Usage(wire.usage.inputTokens, wire.usage.outputTokens))

  /** A decision model over a transport: the tests' scripted Jev. */
  private[zio_decisions] def model(transport: Transport, defaultModel: ModelId = JevLatest): DecisionModel =
    new Model(transport, defaultModel)

  /** Jev, from an explicit API key and model, at `endpoint` (TypeSafe AI's API unless given: a proxy, or a test double
    * speaking the same wire protocol). */
  def layer(apiKey: ApiKey, model: ModelId = JevLatest, endpoint: URL = DefaultEndpoint): URLayer[Client, DecisionModel] =
    ZLayer.fromFunction((client: Client) => new Model(HttpTransport(apiKey, client, endpoint), model))

  /** Reads `TYPESAFE_API_KEY` (required) and `TYPESAFE_DEFAULT_MODEL` (defaults to `jev-latest`): the environment
    * variable names the official Python and JavaScript SDKs use. */
  val live: ZLayer[Client, Error, DecisionModel] =
    ZLayer.fromZIO:
      for
        apiKey <- System.env("TYPESAFE_API_KEY").orDie.map(_.map(_.trim).filter(_.nonEmpty))
                    .someOrFail(Error.MissingApiKey("TYPESAFE_API_KEY"))
        model  <- System.env("TYPESAFE_DEFAULT_MODEL").orDie.map(_.map(_.trim).filter(_.nonEmpty).fold(JevLatest)(ModelId(_)))
        client <- ZIO.service[Client]
      yield new Model(HttpTransport(ApiKey(apiKey), client, DefaultEndpoint), model)

  /** An HTTP error as the library's: a 400 saying the request was too long is [[Error.TooLong]]. */
  def failure(status: Status, body: String): Error = tooLong(Error.fromStatus(status, body))

  /** A 400 saying the request was more than Jev takes, whichever transport reported it, is [[Error.TooLong]]. */
  private def tooLong(error: Error): Error = error match
    case Error.BadRequest(body) if body.contains(TooLongMarker) => Error.TooLong(body)
    case other                                                  => other

  /** Every asked question's wire answer, decoded as its question's kind. */
  private def answers(entries: List[(QuestionId, Question[?])], wire: Wire.SystemOneResponse)
      : Either[Error, Map[QuestionId, DynamicAnswer]] =
    entries.foldLeft[Either[Error, Map[QuestionId, DynamicAnswer]]](Right(Map.empty)):
      case (acc, (qid, question)) =>
        for
          answers    <- acc
          wireAnswer <- wire.answers.get(qid.unwrap).toRight(Error.MissingAnswer(qid))
          answer     <- decode(qid, question, wireAnswer)
        yield answers.updated(qid, answer)

  /** One wire answer, as its question's kind. */
  private[zio_decisions] def decode(qid: QuestionId, question: Question[?], answer: Wire.Answer): Either[Error, DynamicAnswer] =
    (question, answer) match
      case (_: Question.Noul[?], Wire.Answer.Noul(v)) =>
        Probability(v).left.map(Error.MalformedAnswer(qid, _)).map(DynamicAnswer.Noul(_))
      case (_: Question.Choice[?], Wire.Answer.Choice(choice, probabilities, confidence)) =>
        for
          probs <- probabilityMap(probabilities).left.map(Error.MalformedAnswer(qid, _))
          conf  <- Probability(confidence).left.map(Error.MalformedAnswer(qid, _))
        yield DynamicAnswer.Choice(ChoiceAnswer(choice, probs, conf))
      case (_: Question.Score[?], Wire.Answer.Score(score, legend, probabilities, confidence)) =>
        for
          lvlLegend <- levelKeys(legend).left.map(Error.MalformedAnswer(qid, _))
          lvlProbs  <- levelKeys(probabilities).left.map(Error.MalformedAnswer(qid, _))
          probs     <- probabilityMap(lvlProbs).left.map(Error.MalformedAnswer(qid, _))
          conf      <- Probability(confidence).left.map(Error.MalformedAnswer(qid, _))
        yield DynamicAnswer.Score(ScoreAnswer(score, lvlLegend, probs, conf))
      case _ =>
        Left(Error.MalformedAnswer(qid, s"answer type doesn't match the question that was asked: $answer"))

  private def probabilityMap[K](m: Map[K, Double]): Either[String, Map[K, Probability]] =
    m.foldLeft[Either[String, Map[K, Probability]]](Right(Map.empty)):
      case (acc, (k, v)) => acc.flatMap(soFar => Probability(v).map(soFar.updated(k, _)))

  private def levelKeys[A](m: Map[String, A]): Either[String, Map[Int, A]] =
    m.foldLeft[Either[String, Map[Int, A]]](Right(Map.empty)):
      case (acc, (k, v)) =>
        acc.flatMap(soFar => k.toIntOption.toRight(s"non-numeric score level key: '$k'").map(soFar.updated(_, v)))

  /** The live HTTP transport: `POST /v1/systemone` at `endpoint`, with the bearer token. */
  private final class HttpTransport(apiKey: ApiKey, client: Client, endpoint: URL) extends Transport:
    private val authed = client
      .url(endpoint)
      .addHeader(Header.Authorization.Bearer(apiKey: String))
      .addHeader(Header.ContentType(MediaType.application.json))

    private val responseCodec = JsonCodec.schemaBasedBinaryCodec[Wire.SystemOneResponse](Codecs.codecConfig)

    def send(request: Wire.SystemOneRequest): IO[Error, Wire.SystemOneResponse] =
      val body = Body.fromChunk(Chunk.fromArray(request.body.toJson.getBytes(UTF_8)))
      ZIO.scoped:
        authed.post(Path)(body)
          .mapError(Error.Transport.apply)
          .flatMap: response =>
            val status = response.status
            if status.isSuccess then
              response.body.asChunk
                .mapError(Error.Transport.apply)
                .flatMap: bytes =>
                  ZIO.fromEither(responseCodec.decode(bytes))
                    .mapError(de => Error.Unexpected(status, s"Decode failed: ${de.message}"))
            else
              response.body.asString.mapError(Error.Transport.apply).flatMap: text =>
                ZIO.fail(failure(status, text))
