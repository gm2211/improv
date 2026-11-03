package cbr

import java.util.UUID

import com.fasterxml.jackson.annotation.{JsonIgnore, JsonIgnoreProperties, JsonTypeInfo}

@JsonTypeInfo(use = JsonTypeInfo.Id.CLASS, include = JsonTypeInfo.As.PROPERTY, property = "@class")
@JsonIgnoreProperties(Array("map"))
trait CaseSolutionStore[CaseSolution] {

  // Lazily loaded on first use. Kept out of Jackson (a var in a trait leaks into the JSON on
  // Scala 2.13, where the field annotation is not carried over, so the lazy val is also transient).
  @JsonIgnore
  @transient
  private lazy val map: MapStore[String, CaseSolution] = loadMap

  private def getMap = map

  /**
   * Loads the underlying map
   * @return The loaded map
   */
  protected def loadMap: MapStore[String, CaseSolution]

  /**
   * Adds a solution to the store
   * @param caseSolution Solution to be added
   * @return UUID String of the solution
   */
  def addSolution(caseSolution: CaseSolution): String = {
    val newKey = UUID.randomUUID().toString
    getMap.put(newKey, caseSolution)
    newKey
  }

  /**
   * Retrieve a solution by ID
   * @param solutionID ID of the solution to be retrived
   * @return An Option of a solution
   */
  def getSolution(solutionID: String): Option[CaseSolution] = getMap.get(solutionID)

  /**
   * Removes a solution from the store
   * @param solutionID ID of the solution to be removed
   */
  def removeSolution(solutionID: String): Unit = getMap.remove(solutionID)

  /**
   * Removes all entries in the solution store
   */
  def clear(): Unit = getMap.removeAll()

  /**
   * Commits all the changes to the map store
   */
  def commit(): Unit = getMap.commit()
}
