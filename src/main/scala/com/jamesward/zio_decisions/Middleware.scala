package com.jamesward.zio_decisions

import com.jamesward.zio_decisions.Decisions.*
import zio.*
import zio.json.*
import zio.json.ast.Json

/** Composable middleware around every physical [[DecisionModel]] exchange, whichever provider answers it.
  * Composition follows ZIO HTTP: `left ++ right` applies `left` around `right`, and `@@` is an alias for `++`.
  * Requests and responses carry their typed values and their provider-neutral canonical JSON (`body`).
  */
trait Middleware { self =>
  import Middleware.*

  def apply(request: Request, next: IO[Error, Response]): IO[Error, Response]

  final def @@(that: Middleware): Middleware =
    self ++ that

  final def ++(that: Middleware): Middleware =
    Middleware.make: (request, next) =>
      self(request, that(request, next))

  final def layer: URLayer[DecisionModel, DecisionModel] =
    ZLayer.fromFunction: (underlying: DecisionModel) =>
      new DecisionModel:
        val provider: String = underlying.provider
        val defaultModel: ModelId = underlying.defaultModel
        override def name: String = underlying.name

        def decide(request: DecisionModel.Request): IO[Error, DecisionModel.Decision] =
          self(
            Request(request, underlying.defaultModel),
            underlying.decide(request).map(Response(_)),
          ).map(_.decision)
}

object Middleware:
  /** One exchange's request: the typed request and, as `body`, its canonical JSON (with the model it will use). */
  final class Request private[zio_decisions] (val decision: DecisionModel.Request, defaultModel: ModelId):
    lazy val body: Json = decision.json(defaultModel)

  object Request:
    def apply(decision: DecisionModel.Request, defaultModel: ModelId): Request = new Request(decision, defaultModel)

  /** One exchange's response: the decision and, as `body`, its canonical JSON. */
  final case class Response(decision: DecisionModel.Decision):
    lazy val body: Json = decision.json

  final case class LoggingConfig(
    logRequest: Boolean = true,
    logResponse: Boolean = true,
  )

  val identity: Middleware = make((_, next) => next)

  def make(
    f: (Request, IO[Error, Response]) => IO[Error, Response]
  ): Middleware =
    new Middleware:
      def apply(request: Request, next: IO[Error, Response]): IO[Error, Response] =
        f(request, next)

  /** Runs an effect after each exchange without allowing callback defects or
    * self-interruption to change the result.
    */
  def observe(
    f: (Request, Exit[Error, Response], Long) => UIO[Unit]
  ): Middleware =
    make: (request, next) =>
      Clock.nanoTime.flatMap: started =>
        next.onExit: exit =>
          Clock.nanoTime.flatMap: finished =>
            Decisions.notifyBestEffort("Decision middleware"):
              f(request, exit, (finished - started) / 1000000L)

  /** Logs complete request and canonical response bodies. Enable only where
    * logs are approved to contain application state and model output.
    */
  def logging(config: LoggingConfig = LoggingConfig()): Middleware =
    observe: (request, exit, latencyMs) =>
      exit match
        case Exit.Success(response) =>
          val requestText = if config.logRequest then s"; request=${request.body.toJson}" else ""
          val responseText = if config.logResponse then s"; response=${response.body.toJson}" else ""
          ZIO.logInfo(s"Decision exchange completed in ${latencyMs}ms$requestText$responseText")
        case Exit.Failure(cause) =>
          val requestText = if config.logRequest then s"; request=${request.body.toJson}" else ""
          ZIO.logErrorCause(s"Decision exchange failed in ${latencyMs}ms$requestText", cause)

  private[zio_decisions] def fromObserver(observer: ExchangeObserver): Middleware =
    observe: (request, exit, latencyMs) =>
      val outcome = exit match
        case Exit.Success(response) => ExchangeOutcome.Success(response.body)
        case Exit.Failure(cause)    => ExchangeOutcome.Failure(cause)
      observer.observe(ExchangeObservation(request.body, outcome, latencyMs))
