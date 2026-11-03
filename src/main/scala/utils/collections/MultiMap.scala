package utils.collections

import scala.collection.mutable

/** A map from a key to a set of values (replacement for the removed `mutable.MultiMap` mixin). */
type MultiMap[K, V] = mutable.Map[K, mutable.Set[V]]

extension [K, V](multiMap: MultiMap[K, V])
  /** Adds `value` to the set bound to `key`, creating the set if needed. */
  def addBinding(key: K, value: V): MultiMap[K, V] =
    multiMap.getOrElseUpdate(key, mutable.LinkedHashSet.empty[V]) += value
    multiMap

  /** Removes `value` from the set bound to `key`, dropping the key once its set is empty. */
  def removeBinding(key: K, value: V): MultiMap[K, V] =
    multiMap.get(key).foreach { values =>
      values -= value
      if values.isEmpty then multiMap -= key
    }
    multiMap

  def entryExists(key: K, p: V => Boolean): Boolean =
    multiMap.get(key).exists(_.exists(p))
