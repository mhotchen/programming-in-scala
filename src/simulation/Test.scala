package simulation

import TestSimulation._

object Test extends App {
  val input1, input2, sum, carry = new Wire

  probe("sum", sum)
  probe("carry", carry)
  halfAdder(input1, input2, sum, carry)

  input1.setSignal(true)
  run()

  input2.setSignal(true)
  run()
}
