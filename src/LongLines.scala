import scala.io.Source

class LongLines {
  def processFile(fileName: String, width: Int): Unit = {
    def filterLine(line: String) = if (line.length >= width) true else false
    def printLine(line: String) = println(fileName + ":" + line.length + ": " + line)

    Source.fromFile(fileName)
      .getLines
      .filter(filterLine)
      .foreach(printLine)
  }
}

object LongLines {
  def main(args: Array[String]): Unit = {
    val width = args(0).toInt
    val longLines = new LongLines
    for (arg <- args.drop(1)) longLines.processFile(arg, width)
  }
}
