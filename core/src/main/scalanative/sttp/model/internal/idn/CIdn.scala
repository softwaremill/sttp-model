package sttp.model.internal.idn

import scala.scalanative.unsafe.{CInt, CString, Ptr, extern, link, name}

@link("idn2")
@extern
private[idn] object CIdn {
  @name("idn2_to_ascii_8z")
  def toAscii(input: CString, output: Ptr[CString], flags: CInt): CInt = extern

  @name("idn2_strerror")
  def errorMsg(rc: CInt): CString = extern

  @name("idn2_free")
  def free(ptr: Ptr[_]): Unit = extern
}
