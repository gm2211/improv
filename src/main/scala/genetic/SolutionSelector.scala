package genetic

trait SolutionSelector[Elem] {
  /**
   * @param previousSolution What the musician that the solution is for played last, if anything
   * @param candidateSolutions The solutions to choose from
   * @param constraints Conditions that the solution should satisfy
   * @param phrasesByOthers What the other musicians played in the last time slot, i.e. what the solution responds to
   */
  def selectSolution(
    previousSolution: Option[Elem] = None,
    candidateSolutions: List[Elem],
    constraints: List[(Elem) => Boolean],
    phrasesByOthers: Iterable[Elem]): Option[Elem]
}
