package simulation

object Test extends App {
  val circuit = new Circuit with Adders

  val ain = new circuit.Wire("ain", true)
  val bin = new circuit.Wire("bin", false)
  val cin = new circuit.Wire("cin", true)
  val sout = new circuit.Wire("sout")
  val cout = new circuit.Wire("cout")

  circuit.probe(ain)
  circuit.probe(bin)
  circuit.probe(cin)
  circuit.probe(sout)
  circuit.probe(cout)

  circuit.fullAdder(ain, bin, cin, sout, cout)
  circuit.start()
}
