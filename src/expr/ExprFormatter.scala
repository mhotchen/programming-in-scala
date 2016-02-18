package expr

import element.Element

class ExprFormatter {
  private val opGroups = Array(
    Set("|", "||"),
    Set("&", "&&"),
    Set("^"),
    Set("==", "!="),
    Set("<", "<=", ">", ">="),
    Set("+", "-"),
    Set("*", "%")
  )

  private val precedence = (for (i <- opGroups.indices; op <- opGroups(i)) yield op -> i).toMap
  private val unaryPrecedence = opGroups.length
  private val fractionPrecedence = -1

  private def format(e: Expr, enclosingPrecedence: Int): Element = e match {
    case Var(name) => Element.elem(name)
    case Number(num) =>
      def stripDot(s: String) = if (s endsWith ".0") s.substring(0, s.length - 2) else s
      Element.elem(stripDot(num.toString))

    case UnOp(op, arg) => Element.elem(op) beside format(arg, unaryPrecedence)

    case BinOp("/", left, right) =>
      val top = format(left, fractionPrecedence)
      val bottom = format(right, fractionPrecedence)
      val line = Element.elem('-', 1, top.width max bottom.width)
      val frac = top above line above bottom
      if (enclosingPrecedence != fractionPrecedence) frac
      else Element.elem(" ") beside frac beside Element.elem(" ")

    case BinOp(op, left, right) =>
      val opPrecedence = precedence(op)
      val l = format(left, opPrecedence)
      val r = format(right, opPrecedence + 1)
      val oper = l beside Element.elem(" " + op + " ") beside r
      if (enclosingPrecedence <= opPrecedence) oper
      else Element.elem("(") beside oper beside Element.elem(")")
  }

  def format(e: Expr): Element = format(e, 0)
}
