package scells

import scala.swing._
import event._

class Model(val height: Int, val width: Int) extends Evaluator with Arithmetic with Function {
  case class Cell(row: Int, column: Int) extends Component with Publisher {
    private var f: Formula = Empty
    def formula: Formula = f
    def formula_=(f: Formula): Unit = {
      for (c <- references(formula)) deafTo(c)
      this.f = f
      for (c <- references(formula)) listenTo(c)
      value = evaluate(f)
    }

    private var v: Double = 0
    def value: Double = v
    def value_=(w: Double): Unit = {
      if (!(v == w || v.isNaN || w.isNaN)) {
        v = w
        publish(new ValueChanged(this))
      }
    }

    override def toString = formula match {
      case Textual(s) => s
      case _ => value.toString
    }

    reactions += {
      case ValueChanged(_) => value = evaluate(formula)
    }
  }

  val cells = Array.tabulate(height, width) {(h, w) => new Cell(h, w)}
}
