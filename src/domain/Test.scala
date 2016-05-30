package domain

object Test extends App {
  Array("test@test.com", "m@blah.blah.com", "not_valid") foreach {
    case Email("test", Domain("com", "test")) => println("first case")
    case Email(_, Domain(_, "blah", _*)) => println("second case")
    case _ => println("not valid")
  }
}
