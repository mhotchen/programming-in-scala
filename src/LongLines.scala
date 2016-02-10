import scala.io.Source

class LongLines {
  def processFile(fileName: String, width: Int): Unit = {
    val lines = Source.fromFile(fileName)
      .getLines
      .map(line => processLine(line, width))
      .filter(line => if (line == "") false else true)

    lines.foreach(line => println(fileName + ":" + line.length + ": " + line))
  }

  private def processLine(line: String, width: Int) = {
    if (line.length >= width) line else ""
  }
}

object LongLines {
  def main(args: Array[String]): Unit = {
    val width = args(0).toInt
    val longLines = new LongLines
    for (arg <- args.drop(1)) longLines.processFile(arg, width)
  }
}
