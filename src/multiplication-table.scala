val upTo = args(0).toInt
val cellWidth = (upTo * upTo).toString.length + 1

(1 to upTo)
  .map(i => (1 to upTo).map(j => i * j))
  .foreach(row => {
    row.foreach(cell => print(" " * (cellWidth - cell.toString.length) + cell))
    println()
  })
