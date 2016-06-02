package scells

import scala.swing._

object Main extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "SCells Spreadsheet"
    contents = new Spreadsheet(100, 52)
  }
}
