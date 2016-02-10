import java.io.File

class FileMatcher {
  private def filesHere = new File(".").listFiles

  def filesEnding(query: String) = filesMatching(_.endsWith(query))
  def filesContaining(query: String) = filesMatching(_.contains(query))
  def filesRegex(query: String) = filesMatching(_.matches(query))

  def filesMatching(matcher: (String) => Boolean) =
    for (file <- filesHere if matcher(file.getName)) yield file
}
