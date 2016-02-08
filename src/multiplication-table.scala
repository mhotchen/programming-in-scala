val upTo = args(0).toInt
val cellWidth = (upTo * upTo).toString.length + 1

(1 to upTo).foreach(i => {
  (1 to upTo).foreach(j => {
    val prod = (i * j).toString
    print(" " * (cellWidth - prod.length) + prod)
  })
  println()
})