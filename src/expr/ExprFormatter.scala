package expr

import Element.Element

class ExprFormatter {
  private val binaryOpGroups = Array(
    Set("|", "||"),
    Set("&", "&&"),
    Set("^"),
    Set("==", "!="),
    Set("<", "<=", ">", ">="),
    Set("+", "-"),
    Set("*", "%") // divide is missing because we're formatting that vertically
  )

  private val binaryOpPrecedence = (
    for {
      i <- binaryOpGroups.indices
      op <- binaryOpGroups(i)
    } yield op -> i
  ).toMap

  private val unaryOpPrecedence = binaryOpGroups.length
  private val fractionPrecedence = -1

  private def format(expr: Expr, enclosingPrecedence: Int): Element = expr match {
    case Var(name) => Element.elem(name)

    case Number(num) =>
      def stripDot(s: String) = if (s endsWith ".0") s.substring(0, s.length - 2) else s
      Element.elem(stripDot(num.toString))

    case UnOp(op, arg) => Element.elem(op) beside format(arg, unaryOpPrecedence)

    case BinOp("/", left, right) =>
      val top = format(left, fractionPrecedence)
      val bottom = format(right, fractionPrecedence)
      val line = Element.elem('-', 1, top.width max bottom.width)
      val fraction = top above line above bottom
      if (enclosingPrecedence != fractionPrecedence) fraction
      else Element.elem(" ") beside fraction beside Element.elem(" ")

    case BinOp(op, left, right) =>
      val operationPrecedence = binaryOpPrecedence(op)
      val leftElem = format(left, operationPrecedence)
      val rightElem = format(right, operationPrecedence)
      val operation = leftElem beside Element.elem(" " + op + " ") beside rightElem
      if (enclosingPrecedence <= operationPrecedence) operation
      else Element.elem("(") beside operation beside Element.elem(")")
  }

  def format(expr: Expr): Element = format(expr, 0)

  def simplify(expr: Expr): Expr = expr match {
    case UnOp("-", UnOp("-", e)) => simplify(e)
    case UnOp("abs", e@UnOp("abs", _)) => simplify(e)
    case UnOp(op, e) => UnOp(op, simplify(e))

    case BinOp("+", e, Number(0)) => simplify(e)
    case BinOp("-", e, Number(0)) => simplify(e)
    case BinOp("+", l, r) if simplify(l) == simplify(r) => BinOp("*", simplify(l), Number(2))
    case BinOp("-", l, r) if simplify(l) == simplify(r) => Number(0)
    case BinOp("*", e, Number(1)) => simplify(e)
    case BinOp("/", e, Number(1)) => simplify(e)
    case BinOp("/", l, r) if simplify(l) == simplify(r) => Number(1)
    case BinOp(op, l, r) => BinOp(op, simplify(l), simplify(r))

    case _ => expr
  }
}
