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
}

class GASelector[Elem, Chromosome](
      val fitnessFunction: Elem => Double,
      val stoppingCriteria: StoppingCriteria = IterationLimitReached(20),
      val chromosomeGenerator: ChromosomeGenerator[Elem, Chromosome],
      val geneticOperator: GeneticOperator[Chromosome],
      val similarityMeasure: SimilarityMeasure[Elem] = SimilarityMeasures.neutral[Elem]
    ) extends SolutionSelector[Elem] {
  val survivalRate = 2.0/3.0

  /**
   * Fitness of a candidate, weighted by how similar it is to the previous solution (when there is one), so that
   * phrases that continue the musician's previous phrase are favoured over unrelated ones.
   */
  private[genetic] def score(candidate: Elem, previousSolution: Option[Elem]): Double = {
    val similarity = previousSolution.map(similarityMeasure(candidate, _)).getOrElse(1.0)
    scala.math.pow(similarity, GASelector.SimilarityExponent) * fitnessFunction(candidate)
  }


  override def selectSolution(
      previousSolution: Option[Elem],
      candidates: List[Elem],
      constraints: List[(Elem) => Boolean]): Option[Elem] = {

    var population = candidates
    var iterationCount = 0

    while (! shouldTerminate(iterationCount)) {
      val ratedPopulation = candidates.map(candidate => (candidate, score(candidate, previousSolution)))

      val survivedPopulationSize: Int = (ratedPopulation.size * survivalRate).toInt
      val survivors = stochasticSampling(ratedPopulation, survivedPopulationSize)
      population = survivors ++ breed(survivors)
      iterationCount += 1
    }

    Try(population.maxBy(fitnessFunction)).toOption
  }

  //TODO: consider moving this to CollectionUtils
  private def stochasticSampling(ratedPopulation: List[(Elem, Double)], sampleSize: Int): List[Elem] = {
    val maxFitness = ratedPopulation.maxBy(_._2)._2
    val sample = ListBuffer[Elem]()

    while (sample.size < sampleSize) {
      var selectedElement: Option[Elem] = None
      while (selectedElement.isEmpty) {
        val candidate = ratedPopulation(Random.nextInt(ratedPopulation.size))
        if (Random.nextDouble() < candidate._2 / maxFitness){
          selectedElement = Some(candidate._1)
        }
      }
      sample += selectedElement.get
    }
    sample.toList
  }

  private def shouldTerminate(iterationCount: Int): Boolean = stoppingCriteria match {
    case IterationLimitReached(limit) =>
      iterationCount >= limit
  }


  def breed(survivors: List[Elem]): List[Elem] = {
    survivors.combinations(2).collect { case parent1 :: parent2 :: whatever =>
        val chromosome1 = chromosomeGenerator.createChromosome(parent1)
        val chromosome2 = chromosomeGenerator.createChromosome(parent2)

        chromosomeGenerator.fromChromosome(geneticOperator.crossOver(chromosome1, chromosome2));
    }.toList
  }
}
