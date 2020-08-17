package sttp.ws.testing

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.monad.MonadError
import sttp.ws.{WebSocketClosed, WebSocketFrame}

import scala.util.{Failure, Success}

class WebSocketStubTests extends AnyFlatSpec with Matchers with ScalaFutures {
  type Identity[X] = X
  object IdMonad extends MonadError[Identity] {
    override def unit[T](t: T): Identity[T] = t
    override def map[T, T2](fa: Identity[T])(f: T => T2): Identity[T2] = f(fa)
    override def flatMap[T, T2](fa: Identity[T])(f: T => Identity[T2]): Identity[T2] = f(fa)

    override def error[T](t: Throwable): Identity[T] = throw t
    override protected def handleWrappedError[T](rt: Identity[T])(
        h: PartialFunction[Throwable, Identity[T]]
    ): Identity[T] = rt

    override def eval[T](t: => T): Identity[T] = t
  }

  class MyException extends Exception

  "web socket stub" should "return initial Incoming frames on 'receive'" in {
    val frames = List("a", "b", "c").map(WebSocketFrame.text)
    val webSocketStub = WebSocketStub.initialReceive(frames)
    val ws = webSocketStub.build(IdMonad)

    ws.receive() shouldBe WebSocketFrame.text("a")
    ws.receive() shouldBe WebSocketFrame.text("b")
    ws.receive() shouldBe WebSocketFrame.text("c")
  }

  it should "return initial responses on 'receive'" in {
    val okFrame = WebSocketFrame.text("abc")
    val exception = new MyException
    val webSocketStub = WebSocketStub.initialReceiveWith(List(Success(okFrame), Failure(exception)))
    val ws = webSocketStub.build(IdMonad)

    ws.receive() shouldBe WebSocketFrame.text("abc")
    assertThrows[MyException](ws.receive())
  }

  it should "add next incoming frames as reaction to send" in {
    val firstFrame = WebSocketFrame.text("No. 1")
    val secondFrame = WebSocketFrame.text("Second")
    val thirdFrame = WebSocketFrame.text("3")
    val expectedFrame = WebSocketFrame.text("give me more!")
    val webSocketStub = WebSocketStub
      .initialReceive(List(firstFrame))
      .thenRespond {
        case `expectedFrame` => List(secondFrame, thirdFrame)
        case _               => List.empty
      }
    val ws = webSocketStub.build(IdMonad)

    ws.receive() shouldBe WebSocketFrame.text("No. 1")
    assertThrows[IllegalStateException](ws.receive()) // no more stubbed messages
    ws.send(WebSocketFrame.text("not expected"))
    assertThrows[IllegalStateException](ws.receive()) // still, no more stubbed messages
    ws.send(expectedFrame)
    ws.receive() shouldBe WebSocketFrame.text("Second")
    ws.receive() shouldBe WebSocketFrame.text("3")
  }

  it should "add next responses as reaction to send" in {
    val ok = WebSocketFrame.text("ok")
    val exception = new MyException

    val webSocketStub = WebSocketStub.noInitialReceive
      .thenRespondWith(_ => List(Success(ok), Failure(exception)))
    val ws = webSocketStub.build(IdMonad)

    ws.send(WebSocketFrame.text("let's add responses"))
    ws.receive() shouldBe WebSocketFrame.text("ok")
    assertThrows[MyException](ws.receive())
  }

  it should "be closed after sending close frame" in {
    val ok = WebSocketFrame.text("ok")
    val closeFrame = WebSocketFrame.Close(500, "internal error")
    val webSocketStub = WebSocketStub.initialReceive(List(closeFrame, ok))
    val ws = webSocketStub.build(IdMonad)

    ws.send(WebSocketFrame.text("let's add responses"))
    ws.receive() shouldBe closeFrame
    ws.isOpen() shouldBe false
    assertThrows[WebSocketClosed](ws.receive())
  }

  it should "use state to add next responses" in {
    val webSocketStub = WebSocketStub.noInitialReceive
      .thenRespondS(0) {
        case (counter, _) => (counter + 1, List(WebSocketFrame.text(s"No. $counter")))
      }
    val ws = webSocketStub.build(IdMonad)

    ws.send(WebSocketFrame.text("a"))
    ws.send(WebSocketFrame.text("b"))
    ws.send(WebSocketFrame.text("c"))

    ws.receive() shouldBe WebSocketFrame.text("No. 0")
    ws.receive() shouldBe WebSocketFrame.text("No. 1")
    ws.receive() shouldBe WebSocketFrame.text("No. 2")
  }
}
