package tempurature

import scala.swing._
import event._

class Converter extends MainFrame {
  title = "Temperature converter"
  object celsius extends TextField { columns = 5 }
  object fahrenheit extends TextField { columns = 5 }
  object kelvin extends TextField { columns = 5 }
  object rankine extends TextField { columns = 5 }

  contents = new FlowPanel {
    contents += new Label("Celsius: ")
    contents += celsius

    contents += new Label("Fahrenheit")
    contents += fahrenheit

    contents += new Label("Kelvin: ")
    contents += kelvin

    contents += new Label("Rankine: ")
    contents += rankine

    border = Swing.EmptyBorder(10, 10, 10, 10)
  }

  listenTo(celsius, fahrenheit, kelvin)
  reactions += {
    case EditDone(`fahrenheit`) => updateView(kToTemps(fToK(fahrenheit.text.toDouble)))
    case EditDone(`celsius`)    => updateView(kToTemps(cToK(celsius.text.toDouble)))
    case EditDone(`kelvin`)     => updateView(kToTemps(kelvin.text.toDouble))
    case EditDone(`rankine`)    => updateView(kToTemps(rToK(rankine.text.toDouble)))
  }

  private case class Temps(k: Double, c: Double, f: Double, r: Double)

  private def fToK(f: Double) = (f + 459.67) * (5.0/9.0)
  private def kToF(k: Double) = k * (9.0/5.0) - 459.67

  private def cToK(c: Double) = c + 273.15
  private def kToC(k: Double) = k - 273.15

  private def rToK(r: Double) = r * (5.0/9.0)
  private def kToR(k: Double) = k * (9.0/5.0)

  private def kToTemps(k: Double): Temps = Temps(k, kToC(k), kToF(k), kToR(k))

  private def updateView(temps: Temps): Unit = {
    celsius.text = temps.c.formatted("%.2f")
    fahrenheit.text = temps.f.formatted("%.2f")
    kelvin.text = temps.k.formatted("%.2f")
    rankine.text = temps.r.formatted("%.2f")
  }
}

object Converter extends SimpleSwingApplication {
  def top = new Converter
}
