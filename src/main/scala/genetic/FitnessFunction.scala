package genetic

/**
 * What a candidate is a response to, handed to the fitness function along with the candidate, so that it can rate the
 * candidate as an answer rather than in isolation.
 *
 * @param phrasesByOthers What the other musicians played in the last time slot
 * @param previousSolution What the musician that the candidate is for played last, if anything
 */
case class FitnessContext[Elem](phrasesByOthers: List[Elem] = Nil, previousSolution: Option[Elem] = None)

/**
 * Rates how good a candidate is as a response to the context: the higher, the better.
 */
trait FitnessFunction[Elem] extends ((Elem, FitnessContext[Elem]) => Double)

object FitnessFunction {
  /**
   * A fitness function that rates a candidate on its own, whatever it is a response to.
   */
  def ignoringContext[Elem](fitness: Elem => Double): FitnessFunction[Elem] = new FitnessFunction[Elem] {
    override def apply(candidate: Elem, context: FitnessContext[Elem]): Double = fitness(candidate)
  }
}
