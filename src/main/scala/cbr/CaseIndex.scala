package cbr

import cbr.description.{CaseDescription, DescriptionCreator}

trait CaseIndex[Problem] {
  val descriptionCreator: DescriptionCreator[Problem]
  type Solution = Problem

  // Deliberately not an Iterable: jackson-module-scala would then serialise the whole index as a
  // JSON array of case descriptions instead of as an object (and could not read it back).
  def iterator: Iterator[CaseDescription[Problem]]

  def foreach[U](f: CaseDescription[Problem] => U): Unit = iterator.foreach(f)

  def size: Int = iterator.size

  def toIterable: Iterable[CaseDescription[Problem]] = new Iterable[CaseDescription[Problem]] {
    override def iterator: Iterator[CaseDescription[Problem]] = CaseIndex.this.iterator
  }

  /**
   * Finds solutions to the `k` problems most similar to the one provided
   * @param problem Problem for which solutions should be found
   * @param k number of similar problems to be retrieved
   * @return A traversable of up to `k` solutions
   */
  def findSolutionsToSimilarProblems(problem: Problem, k: Int): List[Solution] = {
    findSolutionsToSimilarProblems(descriptionCreator.createCaseDescription(problem), k)
  }
  
  /**
   * Finds solutions to the `k` problems most similar to the one provided
   * @param problemDescription Description of the problem for which solutions should be found
   * @param k number of similar problems to be retrieved
   * @return A traversable of up to `k` solutions
   */
  def findSolutionsToSimilarProblems(problemDescription: CaseDescription[Problem], k: Int): List[Solution]

  /**
   * Adds a solution to a problem to the index
   * @param problem Problem for which the user wants to store a solution
   * @param solution Solution to the problem
   */
  def addSolutionToProblem(problem: Problem, solution: Solution): Unit = {
    addSolutionToProblem(descriptionCreator.createCaseDescription(problem), solution)
  }

  /**
   * Adds a solution to the index
   * @param problemDescription Description of the problem for which the user wants to store a solution
   * @param solution Solution to the problem
   */
  def addSolutionToProblem(problemDescription: CaseDescription[Problem], solution: Solution): Unit

  /**
   * Removes a solution from the index
    * @param problem Problem whose solution is to be removed from the index
   * @return true if successful, false otherwise (maybe trying to remove something that is not in the index)
   */
  def removeSolutionsToProblem(problem: Problem): Boolean = {
    removeSolutionsToProblem(descriptionCreator.createCaseDescription(problem))
  }

  /**
   * Removes a solution from the index
   * @param problemDescription Description of the problem whose solution is to be removed from the index
   * @return true if successful, false otherwise (maybe trying to remove something that is not in the index)
   */
  def removeSolutionsToProblem(problemDescription: CaseDescription[Problem]): Boolean

  /**
   * Remove all entries
   * @return itself so that you can have a sort of fluent api
   */
  def clear(): this.type

  /**
   * Compacts the index potentially removing entries marked for removal
   */
  def compact(): this.type
}
