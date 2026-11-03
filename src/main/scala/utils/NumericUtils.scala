package utils

import scala.math.BigDecimal.RoundingMode

object NumericUtils {

  def max(num1: BigInt, num2: BigInt): BigInt = if (num1 > num2) num1 else num2
  def min(num1: BigInt, num2: BigInt): BigInt = if (num1 < num2) num1 else num2

  def max(num1: BigDecimal, num2: BigDecimal): BigDecimal = if (num1 > num2) num1 else num2
  def min(num1: BigDecimal, num2: BigDecimal): BigDecimal = if (num1 < num2) num1 else num2

  def round(value: BigDecimal, digits: Int): BigDecimal =
    value.setScale(digits, RoundingMode.HALF_EVEN)

  def avg[K](traversable: Iterable[K])(implicit ev: Numeric[K]) = {
    ev.toDouble(traversable.sum) / traversable.size
  }

  def variance[K](traversable: Iterable[K])(implicit num: Numeric[K]): Double = {
    val traversableAvg = avg(traversable)

    traversable.foldLeft(0.0){ (acc, elem) =>
      val distanceFromAvg = num.toDouble(elem) - traversableAvg
      acc + (scala.math.pow(distanceFromAvg, 2) / traversable.size)
    }
  }

  def stdDev[K](traversable: Iterable[K])(implicit num: Numeric[K]): Double = scala.math.sqrt(variance(traversable))

  def normalise[K](numbers: Iterable[K])(implicit ev: Numeric[K]): Iterable[BigDecimal] = {
    val maxVal = numbers.max
    val minVal = numbers.min
    val minMaxDiff = BigDecimal(scala.math.max(ev.toDouble(ev.minus(maxVal, minVal)), 1))

    numbers.map(num => BigDecimal(ev.toDouble(ev.abs(ev.minus(num, minVal)))) / minMaxDiff)
  }

  def normaliseBySum[K](numbers: Iterable[K])(implicit ev: Numeric[K]): Iterable[BigDecimal] = {
    val total: Double = numbers.foldLeft(0.0)((acc, num) => acc + ev.toDouble(num))
    numbers.map(n => BigDecimal(ev.toDouble(n)) / scala.math.max(total, 1))
  }

  def normaliseBySum(numbers: Iterable[BigInt]): Iterable[BigDecimal] = {
    val total = BigDecimal(max(1, numbers.sum))
    numbers.map(n => BigDecimal(n) / total)
  }

  /**
   * Returns a list of tuples of peaks and their relative index. A peak is defined as a value such both its predecessor 
   * and its successor are strictly less than it
   * @param numbers Series for which peaks should be found
   * @return
   */
  def findPeaks(numbers: List[BigDecimal]): List[(BigDecimal, Int)] = {
    for (idx <- (1 until numbers.size - 1).toList
         if numbers(idx - 1) < numbers(idx) &&
            numbers(idx) > numbers(idx + 1)) yield {
      (numbers(idx), idx)
    }
  }

  def combine(array1: Array[Double], array2: Array[Double], op: (Double, Double) => Double): Array[Double] = {
    require(array1.length == array2.length, "Cannot sum two arrays with different lengths")
    array1.zipWithIndex.map{ case (value, idx) => op(value, array2(idx)) }
  }

  def euclid(a1: Array[Double], a2: Array[Double]): Double = {
    require(a1.length == a2.length, "Arrays must have the same length")
    var dist = 0.0

    for ((e1, e2) <- a1.zip(a2)) {
      dist += scala.math.pow(e1 - e2, 2)
    }
    scala.math.sqrt(dist)
  }
}
