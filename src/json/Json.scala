package json

import scala.util.parsing.combinator._

class Json extends JavaTokenParsers {
  def value: Parser[Any] = (
      obj
    | arr
    | stringLiteral
    | floatingPointNumber ^^ (_.toDouble)
    | "null" ^^ (x => null)
    | "true" ^^ (x => true)
    | "false" ^^ (x => false)
    | failure("Invalid value. Maybe you forgot the quotes on a string?")
  )

  def member: Parser[(String, Any)] = stringLiteral~":"~value ^^ {case name~":"~value => (name, value)}
  def obj: Parser[Map[String, Any]] = "{"~>repsep(member, ",")<~"}" ^^ (Map() ++ _)

  def arr: Parser[Any] = "["~>repsep(value, ",")<~"]"
}
