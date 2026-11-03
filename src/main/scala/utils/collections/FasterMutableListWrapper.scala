package utils.collections

import scala.collection.mutable.ListBuffer

class FasterMutableListWrapper[A](private val buffer: ListBuffer[A]) {
  def updateLast(elem: A): Unit = {
    if (buffer.nonEmpty) buffer.update(buffer.size - 1, elem)
  }
}
