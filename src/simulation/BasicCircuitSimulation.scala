package simulation

abstract class BasicCircuitSimulation extends Simulation {
  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  class Wire {
    private var sigVal = false
    private var actions: List[Action] = List()

    def getSignal = sigVal

    def setSignal(s: Boolean) =
      if (s != sigVal) {
        sigVal = s
        actions.foreach(_())
      }

    def addAction(a: Action) = {
      actions = a :: actions
      a()
    }
  }

  def inverter(input: Wire, output: Wire) = {
    def invertAction() = afterDelay(InverterDelay) {
      output.setSignal(!input.getSignal)
    }

    input.addAction(invertAction)
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) = {
    def andAction() = afterDelay(AndGateDelay) {
      output.setSignal(a1.getSignal & a2.getSignal)
    }

    a1.addAction(andAction)
    a2.addAction(andAction)
  }

  def orGate(a1: Wire, a2: Wire, output: Wire) = {
    def orAction() = afterDelay(OrGateDelay) {
      output.setSignal(a1.getSignal | a2.getSignal)
    }

    a1.addAction(orAction)
    a2.addAction(orAction)
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction() = println(name + " " + currentTime + " new-value = " + wire.getSignal)
    wire.addAction(probeAction)
  }
}
