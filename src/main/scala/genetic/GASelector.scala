package genetic

import scala.collection.mutable.ListBuffer
import scala.util.{Random, Try}

object GASelector {
  /**
   * Power the similarity to the previous solution is raised to before it weighs the fitness.
   * Kept at 3.5 for now: the similarity is computed on unscaled features (see improvements.md, section 2),
   * so there is nothing meaningful to tune it against until the features are scaled.
   */
  val SimilarityExponent = 3.5

  /**
   * Number of individuals that compete each time a parent is picked: the best-rated of them becomes the parent.
   */
  val TournamentSize = 2
}

class GASelector[Elem, Chromosome](
      val fitnessFunction: (Elem, FitnessContext[Elem]) => Double,
      val stoppingCriteria: StoppingCriteria = IterationLimitReached(20),
      val chromosomeGenerator: ChromosomeGenerator[Elem, Chromosome],
      val geneticOperator: GeneticOperator[Chromosome],
      val similarityMeasure: SimilarityMeasure[Elem] = SimilarityMeasures.neutral[Elem]
    ) extends SolutionSelector[Elem] {
  val survivalRate = 2.0/3.0

  /**
   * Fitness of a candidate as a response to the context, weighted by how similar it is to the previous solution (when
   * there is one), so that phrases that continue the musician's previous phrase are favoured over unrelated ones.
   */
  private[genetic] def score(candidate: Elem, context: FitnessContext[Elem]): Double = {
    val similarity = context.previousSolution.map(similarityMeasure(candidate, _)).getOrElse(1.0)
    scala.math.pow(similarity, GASelector.SimilarityExponent) * fitnessFunction(candidate, context)
  }

  /**
   * Evolves the candidates for as long as the stopping criteria allow and returns the best-rated individual of the
   * last generation. The population keeps the size of the candidate list throughout: each generation, a share of it
   * (survivalRate) survives and the rest is replaced by children of the survivors.
   *
   * The fitness function rates every individual as a response to what the other musicians played in the last time
   * slot (phrasesByOthers) and to what the musician played last (previousSolution).
   */
  override def selectSolution(
      previousSolution: Option[Elem],
      candidates: List[Elem],
      constraints: List[(Elem) => Boolean],
      phrasesByOthers: Iterable[Elem]): Option[Elem] = {

    val context = FitnessContext(phrasesByOthers.toList, previousSolution)
    val populationSize = candidates.size
    val survivorCount = scala.math.max(1, (populationSize * survivalRate).toInt)
    var ratedPopulation = candidates.map(rate(_, context))
    var iterationCount = 0

    while (ratedPopulation.nonEmpty && ! shouldTerminate(iterationCount)) {
      val survivors = stochasticSampling(ratedPopulation, survivorCount)
      val children = breed(survivors, populationSize - survivors.size).map(rate(_, context))
      ratedPopulation = survivors ++ children
      iterationCount += 1
    }

    Try(ratedPopulation.maxBy(_._2)._1).toOption
  }

  /**
   * Rates an individual with its score. An individual that cannot be scored (NaN) is rated the worst possible.
   */
  private def rate(candidate: Elem, context: FitnessContext[Elem]): (Elem, Double) = {
    val rating = score(candidate, context)
    (candidate, if (rating.isNaN) Double.NegativeInfinity else rating)
  }

  //TODO: consider moving this to CollectionUtils
  private def stochasticSampling(ratedPopulation: List[(Elem, Double)], sampleSize: Int): List[(Elem, Double)] = {
    val maxFitness = ratedPopulation.maxBy(_._2)._2

    if (maxFitness > 0.0 && maxFitness.isFinite) {
      val sample = ListBuffer[(Elem, Double)]()

      while (sample.size < sampleSize) {
        var selectedElement: Option[(Elem, Double)] = None
        while (selectedElement.isEmpty) {
          val candidate = ratedPopulation(Random.nextInt(ratedPopulation.size))
          if (Random.nextDouble() < candidate._2 / maxFitness){
            selectedElement = Some(candidate)
          }
        }
        sample += selectedElement.get
      }
      sample.toList
    } else {
      // Nothing to tell the individuals apart by (every rating is zero, negative or not finite), and the loop above
      // would never accept anyone: pick any of them
      List.fill(sampleSize)(ratedPopulation(Random.nextInt(ratedPopulation.size)))
    }
  }

  private def shouldTerminate(iterationCount: Int): Boolean = stoppingCriteria match {
    case IterationLimitReached(limit) =>
      iterationCount >= limit
  }

  /**
   * Breeds exactly childCount children, each one from a pair of parents picked from the given (rated) individuals by
   * tournament selection.
   */
  def breed(parents: List[(Elem, Double)], childCount: Int): List[Elem] = {
    val candidateParents = parents.toVector

    List.fill(childCount) {
      val chromosome1 = chromosomeGenerator.createChromosome(tournament(candidateParents))
      val chromosome2 = chromosomeGenerator.createChromosome(tournament(candidateParents))

      chromosomeGenerator.fromChromosome(geneticOperator.crossOver(chromosome1, chromosome2))
    }
  }

  private def tournament(ratedPopulation: Vector[(Elem, Double)]): Elem =
    Vector.fill(GASelector.TournamentSize)(ratedPopulation(Random.nextInt(ratedPopulation.size))).maxBy(_._2)._1
}
