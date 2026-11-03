package utils.collections

import scala.collection.mutable.ListBuffer

object BoundedStack {
  def apply[T](bound: Int): BoundedStack[T] = new BoundedStack[T](bound)
}

class BoundedStack[T](private val bound: Int) {
  private val lst = ListBuffer[T]()

  def push(elem: T): Unit = {
    lst += elem
    if (lst.length > bound) {
      lst.remove(0)
    }
  }

  def pop: Option[T] = {
    Option.when(lst.nonEmpty) {
      val elem = lst.last
      lst.remove(lst.size - 1)
      elem
    }
  }

  def peek: Option[T] = {
    Option.when(lst.nonEmpty) {
      lst.last
    }
  }
}
