package tests.utils

import org.scalatest.flatspec.AnyFlatSpec
import tests.TestTags.SlowTest
import utils.collections.{EnhancedIterable, MultiCache}

class CollectionUtilsTest extends AnyFlatSpec {
  "The EnhancedTraversable" should "allow a traversable of pairs w/ the correct type to be converted to a multimap" in {
    // The commented out section won't compile, which is good
    //val traversable = new EnhancedTraversable[(String, String)](List(("a", "1"), ("a", "2")))
    //assert(traversable.pairsToMultiMap[String, Int].get("a").get == Set(1, 2))
    val traversable = new EnhancedIterable[(String, Int)](List(("a", 1), ("a", 2)))
    assert(traversable.pairsToMultiMap[String, Int].get("a").get == Set(1, 2))
  }

  "The MultiCache" should "forget items after the expired timeout" taggedAs SlowTest in {
    val mCache: MultiCache[Long, java.lang.Long] = MultiCache.buildDefault[Long, java.lang.Long](10, 20)
    mCache.addBinding(1, 2L)
    Thread.sleep(1500)
    assert(! mCache.contains(1L))
  }

  "The MultiCache" should "forget items after you get close to the limit" in {
    val mCache: MultiCache[Long, java.lang.Long] = MultiCache.buildDefault[Long, java.lang.Long](100, 20)
    mCache.addBinding(1, 2L)
    (1 to 30).foreach(i => mCache.addBinding(3, i.toLong))
    assert(!mCache.contains(1L))
  }
}
