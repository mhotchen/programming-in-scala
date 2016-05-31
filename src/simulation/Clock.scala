package simulation

import scala.actors.Actor

case class Ping(time: Int)
case class Pong(time: Int, from: Actor)

class Clock extends Actor {
  private var running = false
  private var currentTime = 0
  private var agenda: List[WorkItem] = List()
  private var allSimulants: List[Actor] = List()
  private var busySimulants: Set[Actor] = Set.empty

  def add(sim: Simulant) = {
    allSimulants = sim :: allSimulants
  }

  def act(): Unit = {
    loop {
      if (running && busySimulants.isEmpty) {
        advance()
      }
      reactToOneMessage()
    }
  }

  def reactToOneMessage(): Unit = {
    react {
      case AfterDelay(delay, msg, target) =>
        agenda = insert(agenda, WorkItem(currentTime + delay, msg, target))

      case Pong(time, sim) =>
        assert(time == currentTime)
        assert(busySimulants contains sim)
        busySimulants -= sim

      case Start => running = true

      case Stop =>
        for (sim <- allSimulants) sim ! Stop
        exit()
    }
  }

  def advance(): Unit = {
    if (agenda.isEmpty && currentTime > 0) {
      println("** Agenda empty. Clock exiting at time: " + currentTime)
      this ! Stop
      return
    }

    currentTime += 1
    println("Advancing time to " + currentTime)

    processCurrentEvents()
    for (sim <- allSimulants) sim ! Ping(currentTime)

    busySimulants = allSimulants.toSet
  }

  def processCurrentEvents(): Unit = {
    val todo = agenda.takeWhile(_.time <= currentTime)
    agenda = agenda.drop(todo.length)
    for (WorkItem(time, msg, target) <- todo) {
      assert(time == currentTime)
      target ! msg
    }
  }

  start()

  private def insert(ag: List[WorkItem], item: WorkItem): List[WorkItem] =
    if (ag.isEmpty || item.time < ag.head.time) item :: ag
    else ag.head :: insert(ag.tail, item)
}
