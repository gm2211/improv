package genetic

import utils.ImplicitConversions.toEnhancedIterable
import utils.collections.CollectionUtils

class RandomSelector[Elem] extends SolutionSelector[Elem] {
  override def selectSolution(
      previousSolution: Option[Elem] = None,
      candidates: List[Elem],
      constraints: List[(Elem) => Boolean],
      phrasesByOthers: Iterable[Elem]): Option[Elem] = {
    CollectionUtils.chooseRandom(candidates.filter(constraints))
  }
}
