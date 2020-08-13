package sttp.ws

abstract class WebSocketException(msg: String) extends Exception(msg)

class WebSocketClosed() extends WebSocketException(null)

class WebSocketBufferFull(capacity: Int) extends WebSocketException(s"Buffered $capacity messages")
