package sttp.model.internal

import java.lang.StringBuilder
import java.util.ArrayList

import scala.reflect.ClassTag
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.ArrayBuilder

// ArrayView is just wrapper for Array. It has fast methods 'take', 'drop', 'span' which
// requires only one allocation on heap. These methods are heavily used by UriBuilder.
private[model] class ArrayView[T](arr: Array[T], from: Int, till: Int) {

  def isEmpty: Boolean =
    till <= from

  def nonEmpty: Boolean =
    from < till

  def get(i: Int): T =
    arr(from + i)

  def size: Int =
    till - from

  def headOption: Option[T] =
    if (nonEmpty) {
      Some(arr(from))
    } else {
      None
    }

  def startsWith(t: T): Boolean =
    nonEmpty && arr(from) == t

  def lastOption: Option[T] =
    if (nonEmpty) {
      Some(arr(till - 1))
    } else {
      None
    }

  def foreach(func: T => Unit): Unit = {
    var i = from
    while (i < till) {
      func(arr(i))
      i += 1
    }
  }

  def map[U: ClassTag](func: T => U): ArrayView[U] = {
    val newArray = new Array[U](size)
    var i = 0
    while (i < newArray.size) {
      newArray.update(i, func(arr(i + from)))
      i += 1
    }
    new ArrayView(newArray, 0, newArray.size)
  }

  def flatMapLike(func: T => List[T])(implicit ev: ClassTag[T]): ArrayView[T] = {
    val b = ArrayBuilder.make[T]
    b.sizeHint(2 * size)
    foreach(t => func(t).foreach(b += _))
    val result = b.result()
    new ArrayView(result, 0, result.length)
  }

  def count(pred: T => Boolean): Int = {
    var result: Int = 0
    foreach(t => if (pred(t)) result += 1)
    result
  }

  def mkString(transform: T => String): String = {
    val sb = new StringBuilder(128)
    var i = from
    while (i < till) {
      sb.append(transform(arr(i)))
      i += 1
    }
    sb.toString()
  }

  def mkStringOpt(transform: T => Option[String]): Option[String] = {
    val sb = new StringBuilder(128)
    var empty: Boolean = true
    var i = from
    while (i < till) {
      transform(arr(i)).foreach { str =>
        sb.append(str)
        empty = false
      }
      i += 1
    }
    if (empty) None else Some(sb.toString())
  }

  def indexOf(t: T): Int = {
    var i = from
    while (i < till && arr(i) != t) {
      i += 1
    }
    if (i == till) -1 else i - from
  }

  def indexWhere(pred: T => Boolean): Int = {
    var i = from
    while (i < till && !pred(arr(i))) {
      i += 1
    }
    if (i == till) -1 else i - from
  }

  def take(i: Int): ArrayView[T] =
    new ArrayView(arr, from, from + i)

  def drop(i: Int): ArrayView[T] =
    new ArrayView(arr, from + i, till)

  def dropWhile(pred: T => Boolean): ArrayView[T] = {
    var i = from
    val len = arr.length
    while (i < len && pred(arr(i))) {
      i += 1
    }
    new ArrayView(arr, i, till)
  }

  def span(pred: T => Boolean): (ArrayView[T], ArrayView[T]) = {
    var i = from
    while (i < till && pred(arr(i))) {
      i += 1
    }
    (new ArrayView(arr, from, i), new ArrayView(arr, i + 1, till))
  }

  def shiftLeft: ArrayView[T] =
    new ArrayView(arr, from - 1, till)

  def +:(t: T)(implicit ev: ClassTag[T]): ArrayView[T] = {
    val currentSize = size
    val newArr = new Array[T](currentSize + 1)
    newArr.update(0, t)
    Array.copy(arr, from, newArr, 1, currentSize)
    new ArrayView(newArr, 0, currentSize + 1)
  }

  def toVector: Vector[T] = {
    val b = new VectorBuilder[T]()
    b.sizeHint(size)
    foreach(b += _)
    b.result()
  }

  override def toString: String = {
    val list = new ArrayList[T](size)
    foreach(list.add(_): Unit)
    list.toString()
  }
}

object ArrayView {

  def empty[T: ClassTag]: ArrayView[T] =
    new ArrayView[T](Array.empty[T], 0, 0)

  def apply[T: ClassTag](v: Vector[T]): ArrayView[T] = {
    val arr = v.toArray
    new ArrayView(arr, 0, arr.size)
  }
}

object Singleton {

  def unapply[T](v: ArrayView[T]): Option[T] =
    if (v.size == 1) Some(v.get(0)) else None
}
