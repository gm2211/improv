package storage

import cbr.description.{CaseDescription, DescriptionCreator}
import cbr.{CaseIndex, CaseSolutionStore}
import com.fasterxml.jackson.annotation.{JsonCreator, JsonFormat, JsonProperty}
import net.sf.javaml.core.kdtree.KDTree
import utils.ImplicitConversions.NotNullOption
import utils.{IOUtils, SerialisationUtils}

import scala.collection.JavaConversions._
import scala.language.reflectiveCalls
import scala.math
import scala.util.Try

object KDTreeIndex {
  private val DEFAULT_KDTREE_REBALANCING_THRESHOLD = 1000
  private val DEFAULT_INDEX_RESOURCE: String = "knowledgeBase/caseIndex"
  private val DEFAULT_SOLUTION_STORE_RESOURCE: String = "knowledgeBase/solutionStore"

  def create[Case: Manifest](
      descriptionCreator: DescriptionCreator[Case],
      storeSavePath: String,
      indexSavePath: String): KDTreeIndex[Case] = {
    val store = new MapDBSolutionStore[Case](storeSavePath)
    new KDTreeIndex[Case](store, descriptionCreator, indexSavePath)
  }

  def loadOrCreateDefault[Case : Manifest](
      descriptionCreator: DescriptionCreator[Case]): KDTreeIndex[Case] = {
    val loaded = load[Case](IOUtils.getResourcePath(DEFAULT_INDEX_RESOURCE))
    loaded.getOrElse(createDefault(descriptionCreator))
  }

  def loadDefault[Case : Manifest]: Option[KDTreeIndex[Case]] = load(IOUtils.getResourcePath(DEFAULT_INDEX_RESOURCE))

  def load[Case : Manifest](filename: String): Option[KDTreeIndex[Case]] =
    SerialisationUtils.deserialise[KDTreeIndex[Case]](filename).toOption

  def createDefault[Case : Manifest](descriptionCreator: DescriptionCreator[Case]): KDTreeIndex[Case] = {
    removeDefault()
    val storePath = IOUtils.getResourcePath(DEFAULT_SOLUTION_STORE_RESOURCE)
    val indexPath = IOUtils.getResourcePath(DEFAULT_INDEX_RESOURCE)
    create(descriptionCreator, storePath, indexPath)
  }

  def loadOrCreate[Case : Manifest](
      filename: String,
      descriptionCreator: DescriptionCreator[Case]): KDTreeIndex[Case] = {
    load(filename).getOrElse(createDefault(descriptionCreator))
  }

  private def removeDefault(): Unit = {
    IOUtils.deleteContent(IOUtils.getResourcePath(DEFAULT_INDEX_RESOURCE))
    IOUtils.deleteContent(IOUtils.getResourcePath(DEFAULT_SOLUTION_STORE_RESOURCE))
  }
}


@JsonCreator
// CaseIndex is a Traversable. Newer jackson-module-scala versions treat Scala collections as JSON
// arrays and can't rebuild this class from one; the OBJECT shape keeps it a plain JSON object.
@JsonFormat(shape = JsonFormat.Shape.OBJECT)
class KDTreeIndex[Problem] (
      @JsonProperty("store") private val store: CaseSolutionStore[Problem],
      @JsonProperty("descriptionCreator") override val descriptionCreator: DescriptionCreator[Problem],
      @JsonProperty("path") private var path: String
    )  extends CaseIndex[Problem] with Saveable {
  @JsonProperty("kdTree")
//  @JsonSerialize(using=classOf[KDTreeSerialiser])
  val kdTree = {
    new KDTree[List[String]](
      descriptionCreator.getMaxDescriptionSize,
      KDTreeIndex.DEFAULT_KDTREE_REBALANCING_THRESHOLD)
  }

 override def addSolutionToProblem(problemDescription: CaseDescription[Problem], solution: Solution): Unit = {
    val existingSolutions = searchExact(problemDescription)
    val storedSolutionID = store.addSolution(solution)
    val newSolutions = existingSolutions.getOrElse(List[String]()) :+ storedSolutionID

    if (existingSolutions.isDefined) {
      // Necessary because insert does not overwrite the value of a node
      kdTree.delete(problemDescription.getSignature)
    }

    kdTree.insert(problemDescription.getSignature, newSolutions)
  }

  override def findSolutionsToSimilarProblems(caseDescription: CaseDescription[Problem], k: Int): List[Solution] = {
    val maxNumOfNeighbours = math.min(k, kdTree.getNodeCount)
    val solutionIDs = Try(kdTree.nearest(caseDescription.getSignature, maxNumOfNeighbours).toList)
      .toOption
      .map(_.flatten)
      .getOrElse(List[String]())
    solutionIDs.flatMap(store.getSolution)
  }

  override def removeSolutionsToProblem(caseDescription: CaseDescription[Problem]): Boolean = {
    searchExact(caseDescription).exists { solutionIDs =>
      solutionIDs.foreach(store.removeSolution)
      kdTree.delete(caseDescription.getSignature)
      true
    }
  }

  def searchExact(caseDescription: CaseDescription[Problem]): Option[List[String]] =
    Try(kdTree.search(caseDescription.getSignature)).toNotNullOption

  override def clear(): KDTreeIndex.this.type = {
    this.foreach(node => removeSolutionsToProblem(node.getSignature))
    store.clear()
    this
  }

  /**
   * Compacts the index potentially removing entries marked for removal
   *
   * Note: My modified version of KDTree will already re-balance itself every time
   * a certain threshold of nodes marked as deleted
   */
  def compact(): KDTreeIndex.this.type = {
    kdTree.rebalance()
    this
  }

  override def foreach[U](f: (CaseDescription[Problem]) => U): Unit = {
    kdTree.withFilter(!_.isDeleted).foreach( node => f(node.getKey.getCoord))
  }

  override def save(path: Option[String] = None): Try[Boolean] = {
    store.commit()
    val filePath = path.getOrElse(this.path)
    this.path = filePath
    SerialisationUtils.serialise(this, filePath)
  }
}

