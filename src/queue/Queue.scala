package queue

trait Queue[+T] {
  def head: T
  def tail: Queue[T]
  def enqueue[U >: T](x: U): Queue[U]
}

object Queue {
  private class QueueImpl[+T] (
    private[this] var leading: List[T],
    private[this] var trailing: List[T]
  ) extends Queue[T] {

    private def mirror() = {
      if (leading.isEmpty) {
        while (trailing.nonEmpty) {
          leading = trailing.head :: leading
          trailing = trailing.tail
        }
      }
    }

    def head = {
      mirror()
      leading.head
    }
    def tail = {
      mirror()
      new QueueImpl(leading.tail, trailing)
    }

    override def enqueue[U >: T](x: U): Queue[U] = new QueueImpl[U](leading, x :: trailing)
  }

  def apply[T](xs: T*): Queue[T] = new QueueImpl(xs.toList, Nil)
}
