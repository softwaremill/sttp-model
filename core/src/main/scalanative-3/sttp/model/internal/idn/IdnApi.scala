package sttp.model.internal.idn

import scala.scalanative.libc.stdlib.{malloc, free}
import scala.scalanative.unsafe.{CString, Ptr, Zone, fromCString, sizeof, toCString}

private[model] object IdnApi {
  def toAscii(input: String): String =
    Zone { 
      val output: Ptr[CString] = malloc(sizeof[CString]).asInstanceOf[Ptr[CString]]
      val rc = CIdn.toAscii(toCString(input), output, 0)
      if (rc != 0) {
        val errMsg = CIdn.errorMsg(rc)
        throw new RuntimeException(fromCString(errMsg))
      } else {
        val out = fromCString(!output)
        CIdn.free(!output)
        free(output.asInstanceOf[Ptr[Byte]])
        out
      }
    }
}
