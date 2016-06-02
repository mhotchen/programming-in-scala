package scells

import scala.swing._
import scala.swing.event._

class Spreadsheet(val height: Int, val width: Int) extends ScrollPane {
  val cellModel = new Model(height, width)
  import cellModel._

  val table = new Table(height, width) {
    rowHeight = 25
    autoResizeMode = Table.AutoResizeMode.Off
    showGrid = true
    gridColor = new Color(150, 150, 150)

    override protected def rendererComponent(isSelected: Boolean, focused: Boolean, row: Int, column: Int) =
      if (hasFocus && isSelected) new TextField(userData(row, column))
      else new Label(cells(row)(column).value.toString) {
        xAlignment = Alignment.Right
      }

    reactions += {
      case TableUpdated(_, rows, column) =>
        for (row <- rows) cells(row)(column).formula = FormulaParser.parse(userData(row, column))

      case ValueChanged(cell: Cell) => updateCell(cell.row, cell.column)
    }

    for (row <- cells; cell <- row) listenTo(cell)

    private def userData(row: Int, column: Int): String = {
      val v = this(row, column)
      if (v == null) "" else v.toString
    }
  }

  val rowHeader = new ListView((0 until height) map (_.toString)) {
    fixedCellWidth = 40
    fixedCellHeight = table.rowHeight
  }

  viewportView = table
  rowHeaderView = rowHeader
}
