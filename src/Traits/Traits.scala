trait Philosophical {
  def philosophize() {
    println("I consume memory, therefor I am")
  }
}

class Animal
trait HasLegs {
  def legs: Int
}

class Frog extends Animal with HasLegs with Philosophical {
  override def toString = "green"
  override val legs = 4
}

val frog = new Frog
frog.philosophize()

val phil: Philosophical = new Frog
phil.philosophize()

val legged: HasLegs = new Frog
println(legged.legs)
