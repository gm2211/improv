package tests

import org.scalatest.Tag

object TestTags {
  object SlowTest extends Tag("com.gm2211.improv.test.tags.SlowTest")
  object Manual extends Tag("com.gm2211.improv.test.tags.Manual")
}
