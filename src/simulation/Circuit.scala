package simulation

import scala.actors.Actor

class Circuit {
  val clock = new Clock

  val WireDelay = 1
  val InverterDelay = 2
  val OrGateDelay = 3
  val AndGateDelay = 3

  case class SetSignal(sig: Boolean)
  case class SignalChanged(wire: Wire, sig: Boolean)

  class Wire(name: String, init: Boolean) extends Simulant {
    def this(name: String) = this(name, false)
    def this() = this("Unnamed")

    val clock = Circuit.this.clock
    clock.add(this)

    private var sigVal = init
    private var observers: List[Actor] = List()

    def addObserver(ob: Actor) = observers = ob :: observers

    override def simStarting() = signalObservers()

    def handleMessage(msg: Any) = msg match {
      case SetSignal(s) if s != sigVal =>
        sigVal = s
        signalObservers()

      case SetSignal(_) =>
    }

    override def toString = "Wire(" + name + ")"

    private def signalObservers() =
      for (ob <- observers) clock ! AfterDelay(WireDelay, SignalChanged(this, sigVal), ob)
  }

  abstract class Gate(in1: Wire, in2: Wire, out: Wire) extends Simulant {
    def computeOutput(s1: Boolean, s2: Boolean): Boolean
    val delay: Int

    val clock = Circuit.this.clock
    var s1 = false
    var s2 = false

    in1.addObserver(this)
    in2.addObserver(this)

    def handleMessage(msg: Any) = msg match {
      case SignalChanged(w, sig) =>
        if (w == in1) s1 = sig
        if (w == in2) s2 = sig

        clock ! AfterDelay(delay, SetSignal(computeOutput(s1, s2)), out)
    }
  }

  def orGate(in1: Wire, in2: Wire, out: Wire) = new Gate(in1, in2, out) {
    val delay = OrGateDelay
    def computeOutput(s1: Boolean, s2: Boolean) = s1 || s2
  }

  def andGate(in1: Wire, in2: Wire, out: Wire) = new Gate(in1, in2, out) {
    val delay = AndGateDelay
    def computeOutput(s1: Boolean, s2: Boolean) = s1 && s2
  }

  def notGate(in: Wire, out: Wire) = new Gate(in, new Wire("Dummy"), out) {
    val delay = InverterDelay
    def computeOutput(s1: Boolean, ignored: Boolean) = !s1
  }

  def probe(wire: Wire) = new Simulant {
    override val clock = Circuit.this.clock
    clock.add(this)
    wire.addObserver(this)
    override def handleMessage(msg: Any) = msg match {
      case SignalChanged(w, s) => println("signal " + w + " changed to " + s)
    }
  }

  def start() { clock ! Start }
}

trait Adders extends Circuit {
  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit = {
    val d, e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    notGate(c, e)
    andGate(d, e, s)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire) = {
    val s, c1, c2 = new Wire
    halfAdder(a, cin, s, c1)
    halfAdder(b, s, sum, c2)
    orGate(c1, c2, cout)
  }
}
