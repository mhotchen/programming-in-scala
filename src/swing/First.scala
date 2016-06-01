package swing

import scala.swing._
import scala.swing.event.ButtonClicked

object First extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "My first swing app"
    val button = new Button{ text = "Click me" }
    val label = new Label{ text = "The button hasn't been clicked yet" }
    contents = new BoxPanel(Orientation.Vertical) {
      contents += button
      contents += label
      border = Swing.EmptyBorder(30, 30, 10, 30)
    }

    var clicks = 0
    listenTo(button)
    reactions += {
      case ButtonClicked(b) if b == button =>
        clicks += 1
        label.text = "Number of button clicks: " + clicks
    }
  }
}
