package utils.collections

import scala.collection.mutable
import scala.util.Try

class EnhancedIterable[A](val iterable: Iterable[A]) {
  def groupByMultiMap[K](f: A => K): mutable.MultiMap[K, A] = {
    iterable.foldLeft(CollectionUtils.createHashMultimap[K, A]) { case (mmap, elem) =>
      mmap.addBinding(f(elem), elem)
    }
  }

  def pairsToMultiMap[K : Manifest, V : Manifest](implicit evidence: A =:= (K, V)): mutable.MultiMap[K, V] = {
    iterable.foldLeft(CollectionUtils.createHashMultimap[K, V]) { case (mmap, (key: K, value: V)) =>
      mmap.addBinding(key, value)
    }
  }

  /**
   * Executes a fold left with the starting value being the first value in the list
   * @param defaultValue Value to be returned if the list is empty
   * @param fn
   * @tparam K
   * @return
   */
  def foldLeftWithFstAsDefault[K](defaultValue: K, defValFN: A => K, fn: (K, A) => K): K = {
    val defValue: K = iterable.headOption.map(defValFN).getOrElse(defaultValue)
    iterable.foldLeft[K](defValue)(fn)
  }

  def sumBy[K](defaultValue: K, fn: A => K)(implicit num: Numeric[K]): K =
    iterable.foldLeft[K](defaultValue) { case (acc, elem) => num.plus(acc, fn(elem)) }

  def zipped[K](implicit evidence: A => Iterable[K]): LazyList[List[Option[K]]] = {
    val elemSize = iterable.maxBy{ case e => e.size }.size
    val iterators = iterable.map(_.iterator).toList
    (0 until elemSize).to(LazyList).map{ case i =>
      iterators.map(iter => Try{ iter.next() }.toOption)
    }
  }

  def zipped[K](defaultValue: K)(implicit evidence: A => Iterable[K]): LazyList[List[K]] = {
    val elemSize = iterable.maxBy{ case e => e.size }.size
    val iterators = iterable.map(_.iterator).toList
    (0 until elemSize).to(LazyList).map{ case i =>
      iterators.map(iter => Try{ iter.next() }.getOrElse(defaultValue))
    }
  }

  /**
   * Retuns the number of elements that have type T
   * @tparam T Type of the elements to be counted
   * @return
   */
  def countIfMatchesType[T : Manifest]: Int = {
    var count = 0
    iterable.foreach {
      case elem: T =>
        count += 1
      case _ =>
    }
    count
  }

  def inBounds(idx: Int): Boolean = (0 until iterable.size) contains idx

  def withFilters(filters: Iterable[A => Boolean]): Iterable[A] = {
    var filtered: Iterable[A] = iterable
    filters.foreach(filter => filtered = filtered.filter(filter))
    filtered
  }

  def filter(filters: Iterable[A => Boolean]): Iterable[A] = {
    withFilters(filters)
  }

  def withTypeFilter[T : Manifest]: Iterable[A] = {
    iterable.filter{
      case elem: T => true
      case _ => false
    }
  }

  def filterByType[T : Manifest]: Iterable[T] = {
    iterable.filter{
      case elem: T => true
      case _ => false
    }.asInstanceOf[Iterable[T]]
  }
}
