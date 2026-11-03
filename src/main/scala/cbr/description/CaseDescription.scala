package cbr.description

import cbr.description.features.Feature
import com.fasterxml.jackson.annotation.JsonIgnore
import utils.functional.{FunctionalUtils, MemoizedValue}

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

object CaseDescription {
  implicit def fromArray[Case](array: Array[Double]): CaseDescription[Case] = new CaseDescription[Case] {
    override def getSignature: Array[Double] = array
    override val weightedFeatures: List[(Double, Feature[Case])] = List()
  }
}

trait CaseDescription[Case] {
  val weightedFeatures: List[(Double, Feature[Case])]
  // Lazy, transient and ignored by Jackson: on Scala 2.13 the memoizing closure is a lambda
  // that captures this description, which makes both Jackson and MapDB recurse or fail on it.
  @JsonIgnore @transient lazy val size: MemoizedValue[Int] = FunctionalUtils.memoized(weightedFeatures.foldLeft(0)(_ + _._2.size))

  /**
   * Returns an array of doubles that represents a multi-dimensional point which corresponds to the case Description
   * @return
   */
  def getSignature: Array[Double] = {
    weightedFeatures.foldLeft(new ArrayBuffer[Double]()) { case (buf, (weight, feature)) =>
        buf.appendAll(feature.getSignature.map(weight * _))
        buf
    }.toArray
  }

}
