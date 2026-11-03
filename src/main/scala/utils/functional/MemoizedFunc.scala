package utils.functional

import scala.collection.concurrent

class MemoizedFunc[From, To](private val function: From => To) extends Serializable with ((From) => To) {
  private val results: concurrent.Map[From, To] = concurrent.TrieMap()

  /**
   * Returns the memoized value of the function if available for the input,
   * otherwise it computes it and stores it before returning it
   * @param from Input on which the function should be computed
   * @param refresh Recomputes the value of the function regardless of whether it is stored or not
   * @return The function result
   */
  def apply(from: From, refresh: Boolean = false): To = {
    if (refresh || ! results.contains(from)) {
      results.put(from, function(from))
    }
    results.get(from).get
  }

  /**
   * Updates the cached value To for From
   * @param from input
   * @param to output to be cached
   */
  def update(from: From, to: To) = results.put(from, to)

  /**
   * Drops every cached result. Long-running batch jobs (e.g. the corpus exporter) call this between inputs so that
   * the cache does not keep every input alive.
   */
  def clear(): Unit = results.clear()

  /**
   * Returns another memoized function with a clean cache
   * @return
   */
  def getFreshCopy: MemoizedFunc[From, To] = new MemoizedFunc[From, To](function)

  override def apply(v1: From): To = apply(v1, refresh = false)
}
