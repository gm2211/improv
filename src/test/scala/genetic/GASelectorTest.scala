package genetic

import java.util.concurrent.atomic.AtomicInteger

import genetic.GATestSupport.{CountingCrossover, identityChromosomes, improvingCrossover}
import org.scalatest.flatspec.AnyFlatSpec

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

class GASelectorTest extends AnyFlatSpec {
  private val candidates = (1 to 20).toList

  private def selector(
      fitnessFunction: Int => Double = _.toDouble,
      geneticOperator: GeneticOperator[Int] = improvingCrossover,
      generations: Int = 20) =
    new GASelector[Int, Int](
      fitnessFunction = fitnessFunction,
      stoppingCriteria = IterationLimitReached(generations),
      chromosomeGenerator = identityChromosomes,
      geneticOperator = geneticOperator)

  "A GASelector" should "breed from the offspring of earlier generations, so that the population evolves" in {
    // A child is always one better than its better parent, so after one generation of breeding nothing can beat
    // max(candidates) + 1: a result beyond that has to come from children of children
    val best = selector().selectSolution(None, candidates, Nil)

    assert(best.exists(_ > candidates.max + 1), s"$best did not evolve past ${candidates.max + 1}")
  }

  it should "not evolve past the candidates when told to stop before the first generation" in {
    val best = selector(generations = 0).selectSolution(None, candidates, Nil)

    assert(best.contains(candidates.max))
  }

  it should "breed just enough children in each generation to keep the population size constant" in {
    val generations = 7
    val crossover = new CountingCrossover
    val gaSelector = selector(geneticOperator = crossover, generations = generations)
    val survivors = (candidates.size * gaSelector.survivalRate).toInt

    gaSelector.selectSolution(None, candidates, Nil)

    assert(crossover.calls == generations * (candidates.size - survivors))
  }

  it should "rate each individual in the first generation, then only the children that are added to it" in {
    val generations = 5
    val crossover = new CountingCrossover
    val ratings = new AtomicInteger(0)
    val gaSelector = selector(
      fitnessFunction = { n => ratings.incrementAndGet(); n.toDouble },
      geneticOperator = crossover,
      generations = generations)

    gaSelector.selectSolution(None, candidates, Nil)

    assert(ratings.get == candidates.size + crossover.calls)
  }

  it should "not grow the population however many survive" in {
    // Whatever the number of candidates, the number of children bred per generation never exceeds the population size
    for (size <- List(1, 2, 3, 10, 50)) {
      val crossover = new CountingCrossover
      val result = selector(geneticOperator = crossover, generations = 3)
        .selectSolution(None, (1 to size).toList, Nil)

      assert(result.isDefined, s"no solution for $size candidates")
      assert(crossover.calls <= 3 * size, s"${crossover.calls} children were bred from $size candidates")
    }
  }

  it should "select the only candidate when there is just one" in {
    assert(selector().selectSolution(None, List(5), Nil).isDefined)
  }

  it should "select nothing when there are no candidates" in {
    assert(selector().selectSolution(None, Nil, Nil).isEmpty)
  }

  it should "still select a solution when every candidate has a fitness of zero" in {
    import ExecutionContext.Implicits.global

    val solution = Future(selector(fitnessFunction = _ => 0.0).selectSolution(None, candidates, Nil))

    assert(Await.result(solution, 10.seconds).isDefined)
  }

  it should "still select a solution when some candidates cannot be rated" in {
    import ExecutionContext.Implicits.global

    val solution = Future(selector(fitnessFunction = n => if (n % 2 == 0) Double.NaN else n.toDouble)
      .selectSolution(None, candidates, Nil))

    val selected = Await.result(solution, 10.seconds)
    assert(selected.exists(_ % 2 == 1), s"$selected has no fitness or is not a candidate")
  }

  "breed" should "create exactly the requested number of children" in {
    val gaSelector = selector()
    val parents = candidates.map(c => (c, c.toDouble))

    assert(gaSelector.breed(parents, 0).isEmpty)
    assert(gaSelector.breed(parents, 7).size == 7)
    // 13 parents used to give 13 * 12 / 2 = 78 children
    assert(gaSelector.breed(parents.take(13), 7).size == 7)
  }

  it should "pick parents by tournament, so that fitter parents tend to be picked more often" in {
    val crossover = new GeneticOperator[Int] {
      override def crossOver(e1: Int, e2: Int): Int = e1 // the child is a copy of its first parent
    }
    val gaSelector = selector(geneticOperator = crossover)
    val parents = List((1, 0.1), (2, 100.0))

    val children = gaSelector.breed(parents, 1000)

    // The fitter parent wins a tournament of two unless it is drawn neither time (1 in 4)
    assert(children.count(_ == 2) > children.count(_ == 1) * 2)
  }
}
