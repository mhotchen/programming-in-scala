val upTo = args(0).toInt
val cellWidth = (upTo * upTo).toString.length + 1

List.tabulate(upTo, upTo)(_ + 1 * _ + 1)
  .foreach(row => {
    row.foreach(cell => print(" " * (cellWidth - cell.toString.length) + cell))
    println()
  })
