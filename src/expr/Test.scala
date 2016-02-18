package expr

object Test extends App {
  val f = new ExprFormatter
  def show(e: Expr) = println(f.format(e) + "\n\n")

  val expr1 = BinOp("*", BinOp("/", Number(1), Number(2)),
                         BinOp("+", Var("x"), Number(1)))
  val expr2 = BinOp("+", BinOp("/", Var("x"), Number(2)),
                         BinOp("/", Number(1.5), Var("x")))
  val expr3 = BinOp("*", expr2, expr1)
  val expr4 = BinOp("/", expr3, BinOp("/", expr2, expr1))
  val expr5 = BinOp("*", BinOp("/", expr4, expr3), BinOp("+", expr3, expr2))

  Array(expr1, expr2, expr3, expr4, expr5).foreach(show)
}
