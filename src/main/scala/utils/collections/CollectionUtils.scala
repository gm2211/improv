package utils.collections

import scala.collection.mutable
import scala.math
import scala.util.{Random, Try}

object CollectionUtils {

  def filterNonNumbers(array: Array[Double]): Array[Double] = {
    array.map {
      case d if d.isNaN || d.isInfinity => 0.0
      case d => d
    }
  }


  def mergeMultiMaps[K, V, A](multiMaps: A*)(implicit fn: A => MultiMap[K, V]): MultiMap[K, V] = {
    val mergedMMap = createHashMultimap[K, V]
    for (a <- multiMaps; (key, values) <- fn(a); value <- values) {
      mergedMMap.addBinding(key, value)
    }
    mergedMMap
  }

  def chooseRandom[T](collection: Iterable[T]): Option[T] = {
    def selectRandom(iter: Iterable[T]): Option[T] = {
      if (iter.size <= 0)
        return None

      val idx = Random.nextInt(iter.size)
      iter.view.zipWithIndex.find { case (elem, index) => idx == index }.map(_._1)
    }
    Option(collection).flatMap(selectRandom)
  }

  def randomRange(lowerBound: Int = 1,
    upperBound: Int = 1000): Range = {
    require(lowerBound >= 0, "Only a non-negative lower-bound is accepted")
    require(upperBound >= 0, "Only a non-negative upper-bound is accepted")
    require(upperBound >= lowerBound, "Cannot have a range with upper-bound smaller than lower-bound")

    val upperbound_ = math.min(upperBound, Int.MaxValue)
    val lb = Random.nextInt(upperbound_)
    val ub = lb + Random.nextInt(upperbound_ - lb)

    lb to ub
  }

  def createHashMultimap[Keys, Values]: MultiMap[Keys, Values] =
    mutable.HashMap.empty[Keys, mutable.Set[Values]]

  def mostFrequent[Element](elements: Iterable[Element]): Option[Element] = {
    Try(elements.groupBy(identity).maxBy(_._2.size)._1).toOption
  }

  def print[Elem](elements: Iterable[Elem]): Unit = {
    println(s"{${elements.mkString(",")}}")
  }

  def printNewLine[Elem](elements: Iterable[Elem]): Unit = {
    println(s"{${elements.mkString(",\n")}}")
  }
}



