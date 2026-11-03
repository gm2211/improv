import java.util.UUID

import net.sf.javaml.core.kdtree.KDTree
import org.scalatest.flatspec.AnyFlatSpec
import tests.TestTags.Manual
import utils.{NumericUtils, ProfilingUtils}

import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.jdk.CollectionConverters._

class KDTreePerformanceAnalysis extends AnyFlatSpec {

  "A KDTree" should "outperform a flat db when performing KNN" taggedAs Manual in {
    val numOfDimensions: Int = 100
    val numNeighbours: Int = 10

    def genPoint = Array.fill[Double](numOfDimensions)(Random.nextDouble())

    val kdTree = new KDTree[Any](numOfDimensions)

    val kdTimes = ListBuffer[(Long, Long)]()
    val flatTimes = ListBuffer[(Long, Long)]()

    for (i <- 1 to 20) {
      for (j <- 1 to 10000) {
        kdTree.insert(genPoint, null)
      }

      val target = genPoint
      val (kdTime, _) = ProfilingUtils.timeIt(kdTree.nearest(target, numNeighbours))
      val elems = kdTree.asScala.toList.map(_.getKey.getCoord)
      val (flatTime, _) = ProfilingUtils.timeIt(flatKNN(elems, target, numNeighbours))

      kdTimes.append((elems.size.toLong, kdTime))
      flatTimes.append((elems.size.toLong, flatTime))

      println(s"${i}th iteration")
    }
  }

  def flatKNN(elements: List[Array[Double]], target: Array[Double], k: Int): List[Array[Double]] = {
    def distance = NumericUtils.euclid
    val sortedElements = elements.sortBy(distance(_, target))
    sortedElements.take(k)

      top(elements.map(elem => (elem, distance(elem, target))).toSet, k)
  }

  def top[A](elems: Set[(A, Double)], k: Int): List[A] = {
    require(k > 0)
    val maxElem = elems.maxBy(_._2)
    var lst = List[A]()

    if (k > 1) {
      lst = top(elems.diff(Set(maxElem)), k - 1)
    }

    lst :+ maxElem._1
  }

}
