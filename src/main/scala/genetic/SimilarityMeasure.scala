package genetic

import cbr.MusicalCase
import cbr.description.PhraseDescriptionCreators
import utils.NumericUtils


object SimilarityMeasures {
  /**
   * Turns a distance (0 when two things are identical, growing without bound as they differ) into a similarity
   * in (0, 1]: 1 when they are identical, shrinking towards 0 the further apart they are.
   * @param distance A non-negative distance
   */
  def distanceToSimilarity(distance: Double): Double = 1.0 / (1.0 + distance)

  /**
   * Similarity measure that is derived from a distance function: nearer elements are more similar.
   */
  def fromDistance[T](distance: (T, T) => Double): SimilarityMeasure[T] = new SimilarityMeasure[T] {
    override def similarity(o1: T, o2: T): Double = distanceToSimilarity(distance(o1, o2))
  }

  /**
   * Similarity measure based on the Euclidean distance between the signatures (feature vectors) of two elements.
   */
  def euclidean[T](signature: T => Array[Double]): SimilarityMeasure[T] =
    fromDistance((o1: T, o2: T) => NumericUtils.euclid(signature(o1), signature(o2)))

  /**
   * Similarity measure that finds everything equally similar, i.e. one that leaves a fitness function unaffected.
   */
  def neutral[T]: SimilarityMeasure[T] = new SimilarityMeasure[T] {
    override def similarity(o1: T, o2: T): Double = 1.0
  }

  def musicalCaseSimilarity: SimilarityMeasure[MusicalCase] =
    euclidean[MusicalCase](musicalCase => PhraseDescriptionCreators.getDefault.createCaseDescription(musicalCase).getSignature)
}

/**
 * Measures how alike two elements are: the higher the value, the more alike they are (1 means identical).
 * Distances must be converted (see [[SimilarityMeasures.fromDistance]]) rather than returned as they are.
 */
trait SimilarityMeasure[T] extends ((T, T) => Double) {
  override def apply(o1: T, o2: T): Double = similarity(o1, o2)
  def similarity(o1: T, o2: T): Double
}
