package sttp.model

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import sttp.model.Uri._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 4, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 2, time = 20, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgsAppend = Array("-Xms1G", "-Xmx1G", "-XX:+UnlockDiagnosticVMOptions", "-XX:+DebugNonSafepoints"))
@State(Scope.Benchmark)
class InterpolatorBenchmark {

  @Benchmark
  def simpleUrl: Int = {
    uri"http://example.com/some/path".path.length
  }

  @Benchmark
  def longUrl: Int = {
    val var1 = "path"
    val var2 = "another"
    val var3 = "other"
    val var4 = "zxcv"
    uri"http://example.com/some/$var1/$var2/$var3/$var4/123".path.length
  }

  @Benchmark
  def urlWithQuery: Int = {
    val value3 = "value3"
    val value4 = "value4"
    val value5 = "value5"
    uri"http://example.com/some?key1=value1&key2=value2&key3=$value3&key4=$value4&key5=$value5".path.length
  }
}
