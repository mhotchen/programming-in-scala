package Element

object Element {
  class ArrayElement(val contents: Array[String]) extends Element

  class LineElement(s: String) extends Element {
    val contents = Array(s)
    override val height = if (s.length == 0) 0 else 1
    override val width = s.length
  }

  class UniformElement(ch: Char, override val height: Int, override val width: Int) extends Element {
    override val contents = Array.fill(height)(ch.toString * width)
  }

  def elem(contents: Array[String]) = new ArrayElement(contents)
  def elem(s: String) = new LineElement(s)
  def elem(ch: Char, height: Int, width: Int) = new UniformElement(ch, height, width)
}

abstract class Element {
  def contents: Array[String]

  def height: Int = contents.length
  def width: Int = if (height == 0) 0 else contents(0).length

  def above(that: Element) = Element.elem((this widen that.width).contents ++ (that widen this.width).contents)
  def beside(that: Element) = Element.elem(for ((l, r) <- (this heighten that.height).contents zip (that heighten this.height).contents) yield l + r)

  override def toString = contents.mkString("\n")

  def widen(w: Int): Element =
    if (w <= width) this
    else {
      val left = Element.elem(' ', height, (w - width) / 2)
      val right = Element.elem(' ', height, w - width - left.width)
      left beside this beside right
    } ensuring (w == _.width)

  def heighten(h: Int): Element =
    if (h <= height) this
    else {
      val top = Element.elem(' ', (h - height) / 2, width)
      val bottom = Element.elem(' ', h - height - top.height, width)
      top above this above bottom
    } ensuring (h == _.height)
}

object Spiral {
  val space = Element.elem(" ")
  val corner = Element.elem("+")
  def spiral(nEdges: Int, direction: Int): Element = {
    if (nEdges == 1) corner
    else {
      val sp = spiral(nEdges - 1, (direction + 3) % 4)
      def verticalBar = Element.elem('|', sp.height, 1)
      def horizontalBar = Element.elem('-', 1, sp.width)
      if (direction == 0) (corner beside horizontalBar) above (sp beside space)
      else if (direction == 1) (sp above space) beside (corner above verticalBar)
      else if (direction == 2) (space beside sp) above (horizontalBar beside corner)
      else (verticalBar above corner) beside (space above sp)
    }
  }

  def main(args: Array[String]): Unit = {
    println(Spiral.spiral(args(0).toInt, 0))
  }
}

