package rna

import collection.IndexedSeqLike
import collection.mutable.{ArrayBuffer, Builder}
import collection.generic.CanBuildFrom

final class RNA private (val groups: Array[Int], val length: Int)
  extends IndexedSeq[Base] with IndexedSeqLike[Base, RNA]
{
  import RNA._

  def apply(i: Int): Base = {
    require(i >= 0 && i < length)
    Base.fromInt(groups(i / N) >> (i % N * S) & M)
  }

  override protected[this] def newBuilder: Builder[Base, RNA] = RNA.newBuilder

  // more efficient foreach implementation
  override def foreach[U](f: (Base) => U): Unit = {
    var i = 0
    var b = 0
    while (i < length) {
      b = if (i % N == 0) groups(i / N) else b >>> S
      f(Base.fromInt(b & M))
      i += 1
    }
  }
}

object RNA {
  // number of bits necessary to represent group
  private val S = 2

  // number of groups that fit in an Int
  private val N = 32 / S

  // bitmask to isolate a group
  private val M = (1 << S) - 1

  def fromSeq(buf: Seq[Base]): RNA = {
    val groups = new Array[Int]((buf.length + N - 1) / N)
    for (i <- buf.indices) groups(i / N) |= Base.toInt(buf(i)) << (i % N * S)
    new RNA(groups, buf.length)
  }

  def apply(bases: Base*) = fromSeq(bases)

  def newBuilder: Builder[Base, RNA] = new ArrayBuffer mapResult fromSeq

  implicit def canBuildFrom: CanBuildFrom[RNA, Base, RNA] = new CanBuildFrom[RNA, Base, RNA] {
    def apply(): Builder[Base, RNA] = newBuilder
    def apply(from: RNA): Builder[Base, RNA] = newBuilder
  }
}
