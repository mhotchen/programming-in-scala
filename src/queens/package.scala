package object queens {
  def queens(boardSize: Int): List[List[(Int, Int)]] = {
    def isSafe(queen: (Int, Int), queens: List[(Int, Int)]) = queens forall (!inCheck(queen, _))
    def inCheck(q1: (Int, Int), q2: (Int, Int)) =
      q1._1 == q2._1 ||                          // same row
      q1._2 == q2._2 ||                          // same column
      (q1._1 - q2._1).abs == (q1._2 - q2._2).abs // on diagonal

    def placeQueens(row: Int): List[List[(Int, Int)]] = row match {
      case 0 => List(List())
      case _ => for {
        queens <- placeQueens(row - 1)
        column <- 1 to boardSize
        queen = (row, column)
        if isSafe(queen, queens)
      } yield queen :: queens
    }

    placeQueens(boardSize)
  }
}
