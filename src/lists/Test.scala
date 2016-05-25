package lists

class OrderedTest(val value: String) extends Ordered[OrderedTest] {
  override def compare(that: OrderedTest): Int = {
    value.compareToIgnoreCase(that.value)
  }

  override def toString = value
}

object Test extends App {
  val test1 = List(0, 1, 2, 3, 4, 5, 6, 7)
  val test1concat = List(8, 9, 10)
  val test2 = List("Foo", "Bar")
  val test2concat = List("Baz")
  val test3 = List(new OrderedTest("a"), new OrderedTest("C"), new OrderedTest("B"))
  println(length(test1))
  println(concat(test1, test1concat))
  println(length(test2))
  println(concat(test2, test2concat))
  println(last(test1))
  println(init(test1))
  println(tail(test2concat))
  println(head(test1))
  println(reverse(concat(test1, test1concat)))
  println(reverse(reverse(test1)) == test1)
  println(take(test1, 3))
  println(drop(test1, 3))
  println(apply(test1, 3))
  println(splitAt(test1, length(test1) / 2))
  println(flatten(List(test1, test1concat)))
  println(flatten(List(test2, test2concat)))

  val normalOrderMerge = mergeSort[Int](_ < _)_
  val normalOrderInsert = insertSort[Int](_ < _)_
  val reverseOrderMerge = mergeSort[Int](_ > _)_
  val reverseOrderInsert = insertSort[Int](_ > _)_
  val stringSortMerge = mergeSort[String](_ < _)_
  val stringSortInsert = insertSort[String](_ < _)_

  println(stringSortMerge(concat(test2concat, test2)))
  println(stringSortInsert(concat(test2concat, test2)))
  println(normalOrderMerge(concat(reverse(test1), test1concat)))
  println(normalOrderInsert(concat(reverse(test1), test1concat)))
  println(reverseOrderMerge(test1))
  println(reverseOrderInsert(test1))
  println(normalOrderMerge(reverseOrderMerge(test1)))
  println(normalOrderInsert(reverseOrderInsert(test1)))
  println(orderedMergeSort(test3))
}
