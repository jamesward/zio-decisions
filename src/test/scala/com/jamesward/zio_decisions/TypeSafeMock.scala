package com.jamesward.zio_decisions

import com.jamesward.zio_decisions.Decisions.{Error, ModelId}
import com.jamesward.zio_decisions.typesafe.TypeSafe
import com.jamesward.zio_decisions.typesafe.Wire
import zio.*

import scala.collection.immutable.Queue

/**
 * Test-side scripted Jev: a [[TypeSafe]] decision model over a scripted System One transport. Scripts Jev's behavior as a
 * queue of [[MockBehavior]]s; each `send` consumes one and returns (or
 * fails with) the matching wire response.
 *
 * Unlike `zio-bedrock-converse`'s multi-turn mock, one `send` here is
 * one whole `ask(...).run` — System One answers every registered
 * question in a single round-trip, so one behavior scripts one call.
 *
 * Lives in `src/test` so the production jar carries no test-only code.
 */
object TypeSafeMock:

  sealed trait MockBehavior

  object MockBehavior:
    /** Jev answers every registered question. */
    case class Respond(answers: Map[String, Wire.Answer], usage: Wire.Usage = Wire.Usage(0, 0)) extends MockBehavior

    /** The call fails with the given error. */
    case class Fail(error: Error) extends MockBehavior

    /** Every call from here on is answered from its request (for concurrent callers, whose order isn't fixed). */
    case class Always(answer: Wire.SystemOneRequest => Respond) extends MockBehavior

    /** Like [[Always]], but a request may be answered with an error. */
    case class AlwaysOr(answer: Wire.SystemOneRequest => Either[Error, Respond]) extends MockBehavior

  private case class MockState(
    pending: Queue[MockBehavior],
    requests: Vector[Wire.SystemOneRequest],
  )

  /** A reusable mock layer plus deterministic request capture. */
  final class Tracked private[TypeSafeMock] (
    val layer: ULayer[DecisionModel],
    private val state: Ref[MockState],
  ):
    /** The same layer, named for the orchestration specs. */
    val decisions: ULayer[DecisionModel] = layer
    def requests: UIO[List[Wire.SystemOneRequest]] = state.get.map(_.requests.toList)
    def requestCount: UIO[Int] = state.get.map(_.requests.size)

  /** Builds a mock whose sent wire requests can be inspected and counted. */
  def tracked(behaviors: MockBehavior*): UIO[Tracked] =
    Ref.make(MockState(Queue.from(behaviors.toIndexedSeq), Vector.empty)).map: state =>
      new Tracked(ZLayer.succeed(client(state)), state)

  /** A `DecisionModel` layer whose responses come from `behaviors`. */
  def apply(behaviors: MockBehavior*): ULayer[DecisionModel] =
    ZLayer.fromZIO:
      Ref.make(MockState(Queue.from(behaviors.toIndexedSeq), Vector.empty)).map(client)

  val modelId: ModelId = ModelId("mock")

  private def client(state: Ref[MockState]): DecisionModel = TypeSafe.model(transport(state), modelId)

  private def transport(state: Ref[MockState]): TypeSafe.Transport =
    new TypeSafe.Transport:
      def send(req: Wire.SystemOneRequest): IO[Error, Wire.SystemOneResponse] =
        state.modify: current =>
          val recorded = current.requests :+ req
          if current.pending.isEmpty then
            (None, current.copy(requests = recorded))
          else if current.pending.head.isInstanceOf[MockBehavior.Always] || current.pending.head.isInstanceOf[MockBehavior.AlwaysOr] then
            (Some(current.pending.head), current.copy(requests = recorded))
          else
            val (head, rest) = current.pending.dequeue
            (Some(head), MockState(rest, recorded))
        .flatMap:
          case None =>
            ZIO.fail(Error.Unexpected(
              zio.http.Status.InternalServerError,
              "Mock script exhausted: more `send` rounds than scripted behaviors",
            ))
          case Some(MockBehavior.Respond(answers, usage)) =>
            // `req.body` is the assembled request JSON, not a typed
            // case class — nothing here needs to echo the real
            // requested model back, so just report the mock's own id.
            ZIO.succeed(Wire.SystemOneResponse(model = modelId.unwrap, answers = answers, usage = usage))
          case Some(MockBehavior.Fail(error)) =>
            ZIO.fail(error)
          case Some(MockBehavior.AlwaysOr(answer)) =>
            ZIO.fromEither(answer(req)).map(respond =>
              Wire.SystemOneResponse(model = modelId.unwrap, answers = respond.answers, usage = respond.usage))
          case Some(MockBehavior.Always(answer)) =>
            val respond = answer(req)
            ZIO.succeed(Wire.SystemOneResponse(model = modelId.unwrap, answers = respond.answers, usage = respond.usage))
