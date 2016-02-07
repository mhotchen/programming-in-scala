/*
 * Chapter 3 step 12. Takes in a file name as the argument and prints each line along with the length of that line.
 */

import io.Source

if (args.length > 0) {
  val lines = Source.fromFile(args(0)).getLines.toList
  val maxWidth = widthOfLength(lines.reduceLeft((l, r) => if (l.length > r.length) l else r))
  for (line <- lines) println((" " * (maxWidth - widthOfLength(line))) + line.length + " | " + line)
} else Console.err.println("Please enter a file name")

def widthOfLength(s: String) = s.length.toString.length
