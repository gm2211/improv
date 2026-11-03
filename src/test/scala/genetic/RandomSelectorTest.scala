package genetic

import org.scalatest.flatspec.AnyFlatSpec

class RandomSelectorTest extends AnyFlatSpec {
  "A RandomSelector" should "select one of the candidates that satisfy the constraints, whatever the context" in {
    val selector = new RandomSelector[Int]
    val isEven: Int => Boolean = _ % 2 == 0

    val selected = List.fill(100)(selector.selectSolution(Some(1), List(1, 2, 3, 4, 5, 6), List(isEven), List(99)))

    assert(selected.forall(_.exists(Set(2, 4, 6).contains)))
  }

  it should "select nothing when there are no candidates" in {
    assert(new RandomSelector[Int].selectSolution(None, Nil, Nil, Nil).isEmpty)
  }
}
