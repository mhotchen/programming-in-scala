package simulation

abstract class Simulation {
  type Action = () => Unit
  case class WorkItem(time: Int, action: Action)

  private var current = 0
  def currentTime: Int = current

  private var agenda: List[WorkItem] = List()

  private def insert(ag: List[WorkItem], item: WorkItem): List[WorkItem] =
    if (ag.isEmpty || item.time < ag.head.time) item :: ag
    else ag.head :: insert(ag.tail, item)

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    agenda = insert(agenda, WorkItem(currentTime + delay, () => block))
  }

  private def next() {
    (agenda: @unchecked) match {
      case item :: rest =>
        agenda = rest
        current = item.time
        item.action()
    }
  }

  def run() {
    afterDelay(0) {
      println("-- simulation started, time = " + currentTime)
    }

    while (agenda.nonEmpty) next()
  }
}
