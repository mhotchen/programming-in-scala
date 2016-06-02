package scells

import scala.util.parsing.combinator._

object FormulaParser extends RegexParsers {
  def ident:       Parser[String]      = """[a-zA-Z_]\w*""".r
  def cell:        Parser[Coord]       = """[A-Za-z]+\d+""".r                ^^ stringToCoord
  def range:       Parser[Range]       = cell~":"~cell                       ^^ {case c1~":"~c2 => Range(c1, c2)}
  def number:      Parser[Number]      = """-?\d+(\.\d*)?""".r               ^^ (d => Number(d.toDouble))
  def application: Parser[Application] = ident~"("~repsep(arg, ",")~")"      ^^ {case f~"("~p~")" => Application(f.toLowerCase, p)}
  def arg:         Parser[Formula]     = range | cell | number | application
  def textual:     Parser[Textual]     = """[^=].*""".r                      ^^ Textual
  def formula:     Parser[Formula]     = number | textual | "="~>application

  def parse(input: String): Formula = parseAll(formula, input) match {
    case Success(e, _) => e
    case f: NoSuccess => Textual("[" + f.msg + "]")
  }

  private def stringToCoord(s: String): Coord = {
    val row = """\d+$""".r.findFirstIn(s) match {
      case Some(r) => r.toInt
      case None => throw new RuntimeException // in theory will never happen
    }

    val column = """^[A-Za-z]+""".r.findFirstIn(s) match {
      case Some(c) => (-26 /: c.toCharArray) (_ + 26 + _.toUpper - 'A')
      case None => throw new RuntimeException // in theory will never happen
    }

    Coord(row, column)
  }
}
