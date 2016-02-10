import scala.io.Source

class LongLines {
  def processFile(fileName: String, width: Int): Unit = {
    Source.fromFile(fileName)
      .getLines
      .filter(_.length >= width)
      .foreach(line => println(fileName + ":" + line.length + ": " + line))
  }
}

object LongLines {
  def main(args: Array[String]): Unit = {
    val processFile = (new LongLines).processFile(_: String, args(0).toInt)
    args drop 1 foreach processFile
  }
}
