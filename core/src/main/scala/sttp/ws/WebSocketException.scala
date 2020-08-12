package sttp.ws

trait WebSocketException

class WebSocketClosed() extends Exception with WebSocketException

class WebSocketBufferFull() extends Exception with WebSocketException
