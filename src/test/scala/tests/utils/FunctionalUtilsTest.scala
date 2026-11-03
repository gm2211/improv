package tests.utils

import org.scalatest.flatspec.AnyFlatSpec
import utils.functional.FunctionalUtils

class FunctionalUtilsTest extends AnyFlatSpec {
  def foo(i: Int, s: String) = s"$s$i"
  val fooCurr = (i: Int) => (s: String) => s"$s$i"
  
  "Flip" should "take a method with two parameters and return a function which has the parameters swapped" in {
    val x = FunctionalUtils.flip(foo)("a", 2)
    val y = foo(2, "a")
    assert(x == y)
  }

  "Flip" should "take a curried function with two parameters and return a curried function which has the parameters swapped" in {
    val x = FunctionalUtils.flip(fooCurr)("a")(2)
    val y = fooCurr(2)("a")
    assert(x == y)
  }
}
