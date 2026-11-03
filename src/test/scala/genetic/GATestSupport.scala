package genetic

import java.util.concurrent.atomic.AtomicInteger

/**
 * Building blocks for exercising GASelector on plain Ints
 */
object GATestSupport {
  def identityChromosomes: ChromosomeGenerator[Int, Int] = new ChromosomeGenerator[Int, Int] {
    override def createChromosome(elem: Int): Int = elem
    override def fromChromosome(chromosome: Int): Int = chromosome
  }

  /**
   * A crossover that always produces a child that is better than both parents (when fitter means bigger)
   */
  def improvingCrossover: GeneticOperator[Int] = new GeneticOperator[Int] {
    override def crossOver(e1: Int, e2: Int): Int = scala.math.max(e1, e2) + 1
  }

  def averagingCrossover: GeneticOperator[Int] = new GeneticOperator[Int] {
    override def crossOver(e1: Int, e2: Int): Int = (e1 + e2) / 2
  }

  class CountingCrossover extends GeneticOperator[Int] {
    private val counter = new AtomicInteger(0)
    def calls: Int = counter.get
    override def crossOver(e1: Int, e2: Int): Int = {
      counter.incrementAndGet()
      scala.math.max(e1, e2) + 1
    }
  }
}
