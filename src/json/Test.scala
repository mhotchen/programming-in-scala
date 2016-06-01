package json

import java.io.FileReader

object Test {
  def main(args: Array[String]): Unit = {
    val parser = new Json
    val addressBook = new FileReader("json/tests/address-book.json")
    println(parser.parseAll(parser.value, addressBook))
  }

}
