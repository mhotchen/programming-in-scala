package expr

object Test extends App {
  val f = new ExprFormatter
  val e1 = BinOp("*", BinOp("/", Number(2), Number(1)),
                      BinOp("+", Var("x"),  Number(1)))
  val e2 = BinOp("+", BinOp("/", Var("x"),  Number(1)),
                      BinOp("/", Number(1.5), Var("y")))
  val e3 = BinOp("/", e1, e1)
  val e4 = BinOp("/", e1, e2)

  for (e <- Array(e1, e2, e3, e4)) {
    println("Unsimplified")
    println(f.format(e))
    println("\n\n")
    println("Simplified")
    println(f.format(f.simplify(e)))
    println("\n\n\n\n")
  }
}
