package sttp.capabilities.zio

import sttp.capabilities.Streams
import zio.blocking.Blocking
import zio.stream.{Stream, Transducer, ZStream, ZTransducer}

trait ZioStreams extends Streams[ZioStreams] {
  override type BinaryStream = Stream[Throwable, Byte]
  override type Pipe[A, B] = Transducer[Throwable, A, B]
}
object ZioStreams extends ZioStreams

trait BlockingZioStreams extends Streams[BlockingZioStreams] {
  override type BinaryStream = ZStream[Blocking, Throwable, Byte]
  override type Pipe[A, B] = ZTransducer[Blocking, Throwable, A, B]
}
object BlockingZioStreams extends BlockingZioStreams
