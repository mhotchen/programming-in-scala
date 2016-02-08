import java.io.File
import scala.io.Source

val dir = args(0)

def fileAndLine = for (
  file <- new File(dir).listFiles if file.isFile && file.canRead;
  line <- Source.fromFile(file).getLines.toList
) yield (file, line)

val longestLine = fileAndLine
  .reduceLeft(
    (l, r) =>
      if (getPrefix(l._1, l._2).length > getPrefix(r._1, r._2).length) l else r
  )

val longestPrefix = getPrefix(longestLine._1, longestLine._2).length

fileAndLine.foreach(f => println(padString(getPrefix(f._1, f._2), longestPrefix) + " " + f._2))

def padString(string: String, max: Int, char: Char = ' ') = char.toString * (max - string.length) + string
def getPrefix(file: File, line: String) = file.getName + ":" + line.length
