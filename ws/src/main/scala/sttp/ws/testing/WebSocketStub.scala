package sttp.ws.testing

import sttp.model.Headers
import sttp.monad.MonadError
import sttp.ws.{WebSocket, WebSocketClosed, WebSocketFrame}

import scala.util.{Failure, Success, Try}

/**
  * A stub for websockets that uses a queue of frames which are returned when the client calls
  * [[WebSocket.receive]].
  *
  * New messages can be added to queue in reaction to [[WebSocket.send]] being invoked, by specifying the
  * behavior using one of the [[thenRespond]] variants.
  *
  * For more complex cases, please provide your own implementation of [[WebSocket]].
  */
class WebSocketStub[S](
    initialResponses: List[Try[WebSocketFrame]],
    initialState: S,
    makeNewResponses: (S, WebSocketFrame) => (S, List[Try[WebSocketFrame]])
) {
  /**
    * Creates a stub that has the same initial receive frames, but replaces the function that adds responses to be
    * received in reaction to [[WebSocket.send]] being invoked.
    */
  def thenRespond(
      addReceived: WebSocketFrame => List[WebSocketFrame]
  ): WebSocketStub[Unit] = thenRespondWith(f => addReceived(f).map(Try(_)))

  /**
    * Creates a stub that has the same initial receive frames, but replaces the function that adds responses to be
    * received in reaction to [[WebSocket.send]] being invoked.
    *
    * More powerful version of [[thenRespond()]] which allows receiving to fail with an exception.
    */
  def thenRespondWith(
      addReceived: WebSocketFrame => List[Try[WebSocketFrame]]
  ): WebSocketStub[Unit] =
    new WebSocketStub(
      initialResponses,
      (),
      (_, frame) => ((), addReceived(frame))
    )

  /**
    * Creates a stub that has the same initial responses, but replaces the function that adds responses to be
    * received in reaction to [[WebSocket.send]] being invoked.
    *
    * More powerful version of [[thenRespond()]], as the given function can additionally use state and implement stateful
    * logic for computing response messages.
    */
  def thenRespondS[S2](initial: S2)(
      onSend: (S2, WebSocketFrame) => (S2, List[WebSocketFrame])
  ): WebSocketStub[S2] = thenRespondWithS(initial) { (s, f) =>
    val (s2, lf) = onSend(s, f)
    (s2, lf.map(Try(_)))
  }

  /**
    * Creates a stub that has the same initial responses, but replaces the function that adds responses to be
    * received in reaction to [[WebSocket.send]] being invoked.
    *
    * More powerful version of:
    * - [[thenRespond()]], as the given function can additionally use state and implement stateful
    * logic for computing response messages.
    * - [[thenRespondS()]] which allows receiving to fail with an exception.
    */
  def thenRespondWithS[S2](initial: S2)(
      onSend: (S2, WebSocketFrame) => (S2, List[Try[WebSocketFrame]])
  ): WebSocketStub[S2] = new WebSocketStub(initialResponses, initial, onSend)

  def build[F[_]](implicit m: MonadError[F]): WebSocket[F] =
    new WebSocket[F] {
      private var state: S = initialState
      private var _isOpen: Boolean = true
      private var responses = initialResponses

      override def monad: MonadError[F] = m
      override def isOpen(): F[Boolean] = monad.unit(_isOpen)

      override def receive(): F[WebSocketFrame] =
        synchronized {
          if (_isOpen) {
            responses.headOption match {
              case Some(Success(close: WebSocketFrame.Close)) =>
                _isOpen = false
                monad.unit(close)
              case Some(Success(response)) =>
                responses = responses.tail
                monad.unit(response)
              case Some(Failure(e)) =>
                _isOpen = false
                monad.error(e)
              case None =>
                monad.error(new IllegalStateException("Unexpected 'receive', no more prepared responses."))
            }
          } else {
            monad.error(new WebSocketClosed())
          }
        }

      override def send(frame: WebSocketFrame, isContinuation: Boolean): F[Unit] =
        monad.flatten(monad.eval {
          synchronized {
            if (_isOpen) {
              val (newState, newResponses) = makeNewResponses(state, frame)
              responses = responses ++ newResponses
              state = newState
              monad.unit(())
            } else {
              monad.error(new Exception("WebSocket is closed."))
            }
          }
        })

      override val upgradeHeaders: Headers = Headers(Nil)
    }
}

object WebSocketStub {
  /**
    * Creates a stub which will return the given responses when [[WebSocket.receive]] is called by the client.
    * More messages can be enqueued to be returned by the stub in response to [[WebSocket.send]] by subsequently
    * calling one of the [[WebSocketStub.thenRespond]] methods.
    */
  def initialReceiveWith(
      events: List[Try[WebSocketFrame]]
  ): WebSocketStub[Unit] = {
    new WebSocketStub(events, (), (_, _) => ((), List.empty))
  }

  /**
    * Creates a stub which will return the given messages when [[WebSocket.receive]] is called by the client.
    * More messages can be enqueued to be returned by the stub in response to [[WebSocket.send]] by subsequently
    * calling one of the [[WebSocketStub.thenRespond]] methods.
    */
  def initialReceive(
      messages: List[WebSocketFrame]
  ): WebSocketStub[Unit] = {
    initialReceiveWith(messages.map(m => Success(m)))
  }

  /**
    * Creates a stub which won't return any initial frames when [[WebSocket.receive]] is called by the client.
    * More messages can be enqueued to be returned by the stub in response to [[WebSocket.send]] by subsequently
    * calling one of the [[WebSocketStub.thenRespond]] methods.
    */
  def noInitialReceive: WebSocketStub[Unit] = initialReceiveWith(List.empty)
}
