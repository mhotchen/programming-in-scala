package lists

object Test extends App{
  val test1 = List(0, 1, 2, 3, 4, 5, 6, 7)
  val test1concat = List(8, 9, 10)
  val test2 = List("Foo", "Bar")
  val test2concat = List("Baz")
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
}
