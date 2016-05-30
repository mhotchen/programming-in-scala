package queens

object Test extends App {
  val boardSize = args(0).toInt
  val placedQueens = queens(boardSize)
  println("Queens on board of size " + boardSize)
  placedQueens foreach (println(_))
}
