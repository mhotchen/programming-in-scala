/**
  * Reinventing built in list things for fun
  */
package object lists {
  def concat[T](l: List[T], r: List[T]): List[T] = l match {
    case List() => r
    case firstValue :: restOfLeft => firstValue :: concat(restOfLeft, r)
  }

  def length[T](list: List[T]): Int = list match {
    case List() => 0
    case first :: rest => 1 + length(rest)
  }

  def head[T](list: List[T]): T = list match {
    case List() => throw new Exception("Not available on empty lists")
    case first :: rest => first
  }

  def last[T](list: List[T]): T = list match {
    case List() => throw new Exception("Not available on empty lists")
    case List(e) => e
    case first :: rest => last(rest)
  }

  def init[T](list: List[T]): List[T] = list match {
    case List() => throw new Exception("Not available on empty lists")
    case List(first, _) => List(first)
    case first :: rest => first :: init(rest)
  }

  def tail[T](list: List[T]): List[T] = list match {
    case List() => throw new Exception("Not available on empty lists")
    case first :: rest => rest
  }

  def reverse[T](list: List[T]): List[T] = list match {
    case List() => list
    case List(e) => list
    case first :: rest => concat(reverse(rest), List(first))
  }

  def take[T](list: List[T], count: Int): List[T] = count match {
    case 0 => List()
    case 1 => List(head(list))
    case n if n <= 0 => throw new Exception("count must be greater than 0")
    case n => head(list) :: take(tail(list), n - 1)
  }

  def drop[T](list: List[T], count: Int): List[T] = count match {
    case 0 => list
    case n if n <= 0 => throw new Exception("count must be greater than 0")
    case n => drop(tail(list), n - 1)
  }

  def splitAt[T](list: List[T], index: Int): (List[T], List[T]) = (take(list, index), drop(list, index))

  def apply[T](list: List[T], index: Int): T = head(drop(list, index))

  def flatten[T](list: List[List[T]]): List[T] = list match {
    case List() => List[T]()
    case List(item) => item
    case head :: rest => concat(head, flatten(rest))
  }

  def insertSort[T](comp: (T, T) => Boolean)(list: List[T]): List[T] = list match {
    case List() => list
    case List(_) => list
    case head :: rest =>
      def insert(item: T, list: List[T]): List[T] = list match {
        case List() => item :: list
        case insertHead :: _ if comp(item, insertHead) => item :: list
        case insertHead :: insertRest => insertHead :: insert(item, insertRest)
      }

      insert(head, insertSort(comp)(rest))
  }

  def mergeSort[T](comp: (T, T) => Boolean)(list: List[T]): List[T] = {
    def merge(l: List[T], r: List[T]): List[T] = (l, r) match {
      case (Nil, _) => r
      case (_, Nil) => l
      case (x :: restL, y :: restR) if comp(x, y) => x :: merge(restL, r)
      case (x :: restL, y :: restR)               => y :: merge(l, restR)
    }

    val midPoint = length(list) / 2
    if (midPoint == 0) list
    else {
      val (l, r) = splitAt(list, midPoint)
      merge(mergeSort(comp)(l), mergeSort(comp)(r))
    }
  }

  def orderedMergeSort[T <: Ordered[T]](list: List[T]): List[T] = {
    def merge(l: List[T], r: List[T]): List[T] = (l, r) match {
      case (Nil, _) => r
      case (_, Nil) => l
      case (x :: restL, y :: restR) if x < y => x :: merge(restL, r)
      case (x :: restL, y :: restR)          => y :: merge(l, restR)
    }

    val midPoint = length(list) / 2
    if (midPoint == 0) list
    else {
      val (l, r) = splitAt(list, midPoint)
      merge(orderedMergeSort(l), orderedMergeSort(r))
    }
  }
}
