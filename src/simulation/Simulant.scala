package simulation

import scala.actors.Actor

case class WorkItem(time: Int, msg: Any, target: Actor)
case class AfterDelay(delay: Int, msg: Any, target: Actor)

case object Start
case object Stop

trait Simulant extends Actor {
  val clock: Clock
  def handleMessage(msg: Any)
  def simStarting() {}
  def act() {
    loop {
      react {
        case Stop => exit()
        case Ping(time) =>
          if (time == 1) simStarting()
          clock ! Pong(time, this)
        case msg => handleMessage(msg)
      }
    }
  }

  start()
}

