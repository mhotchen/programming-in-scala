import ChecksumAccumulator.calculate

object Application extends App {
  for (arg <- args) println(arg + ": " + calculate(arg))
}
