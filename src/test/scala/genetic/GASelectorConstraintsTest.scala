package genetic

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger

import cbr.MusicalCase
import genetic.GATestSupport.{CountingCrossover, identityChromosomes, improvingCrossover}
import instruments.InstrumentType.{BASS, PIANO}
import org.scalatest.flatspec.AnyFlatSpec
import representation.{Jazz, Note, Phrase}

import scala.concurrent.duration.{DurationInt, SECONDS}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.jdk.CollectionConverters.*
import scala.util.Random

/**
 * GASelector has to honour the constraints like RandomSelector does: the solution it returns satisfies all of them.
 *
 * The properties checked here (the result satisfies the constraints, nothing that fails them is rated) hold on every
 * run, whatever the random choices of the algorithm: the tests that rely on chance only check that the run was a
 * meaningful one (for instance that the children are not all dropped).
 */
class GASelectorConstraintsTest extends AnyFlatSpec {
  private val candidates = (1 to 20).toList
  private val isEven: Int => Boolean = _ % 2 == 0
  private val isOdd: Int => Boolean = _ % 2 == 1
  private val Runs = 100

  /**
   * One or two above the better parent: always an improvement, and (with even parents) even on a coin toss
   */
  private val coinTossCrossover: GeneticOperator[Int] = new GeneticOperator[Int] {
    override def crossOver(e1: Int, e2: Int): Int = scala.math.max(e1, e2) + 1 + Random.nextInt(2)
  }

  private def selector(
      fitnessFunction: Int => Double = _.toDouble,
      geneticOperator: GeneticOperator[Int] = improvingCrossover,
      generations: Int = 30) =
    new GASelector[Int, Int](
      fitnessFunction = FitnessFunction.ignoringContext(fitnessFunction),
      stoppingCriteria = IterationLimitReached(generations),
      chromosomeGenerator = identityChromosomes,
      geneticOperator = geneticOperator)

  private def select(gaSelector: GASelector[Int, Int], constraints: List[Int => Boolean], runs: Int = Runs) =
    List.fill(runs)(gaSelector.selectSolution(None, candidates, constraints, Nil))

  "A GASelector" should "only select a candidate that satisfies the constraint, across many generations of crossover" in {
    val results = select(selector(geneticOperator = coinTossCrossover), List(isEven))

    assert(results.forall(_.exists(isEven)), s"$results has a solution that fails the constraint")
    // Half of the children are even, so some of them make it to the top: the children are bred, not just dropped
    assert(results.exists(_.exists(_ > candidates.max)), "no child made it to the result")
  }

  it should "only select a candidate that satisfies the constraint when none of the children do" in {
    import ExecutionContext.Implicits.global

    // The crossover turns even parents into an odd child, every time
    val results = Future(select(selector(geneticOperator = improvingCrossover), List(isEven)))

    val selected = Await.result(results, 10.seconds)
    assert(selected.forall(_.exists(candidates.filter(isEven).contains)), s"$selected has a solution that fails the constraint")
  }

  it should "not rate any individual that fails the constraint, children included" in {
    val rated = new ConcurrentLinkedQueue[Int]()
    val gaSelector = selector(
      fitnessFunction = { n => rated.add(n); n.toDouble },
      geneticOperator = coinTossCrossover)

    select(gaSelector, List(isEven), runs = 20)

    assert(rated.asScala.nonEmpty)
    assert(rated.asScala.forall(isEven), s"${rated.asScala.filterNot(isEven).toSet} was rated")
  }

  it should "satisfy all the constraints, not just one of them" in {
    val results = select(selector(geneticOperator = coinTossCrossover), List(isEven, _ > 10))

    assert(results.forall(_.exists(n => isEven(n) && n > 10)), s"$results has a solution that fails a constraint")
  }

  it should "start from the best of the candidates that satisfy the constraints" in {
    // No generations: the solution is the best-rated candidate that passes
    val noGenerations = selector(generations = 0)

    assert(noGenerations.selectSolution(None, candidates, Nil, Nil).contains(20))
    assert(noGenerations.selectSolution(None, candidates, List(_ < 7), Nil).contains(6))
    assert(noGenerations.selectSolution(None, candidates, List(isOdd, _ < 7), Nil).contains(5))
  }

  it should "select the only candidate that satisfies the constraints" in {
    val results = select(selector(), List(isEven, _ > 18))

    assert(results.forall(_.contains(20)), s"$results is not 20")
  }

  it should "select nothing when no candidate satisfies the constraints, like RandomSelector" in {
    val crossover = new CountingCrossover
    val ratings = new AtomicInteger(0)
    val gaSelector = selector(fitnessFunction = { n => ratings.incrementAndGet(); n.toDouble }, geneticOperator = crossover)

    assert(gaSelector.selectSolution(None, candidates, List(_ > 100), Nil).isEmpty)
    assert(gaSelector.selectSolution(None, candidates, List(isEven, isOdd), Nil).isEmpty)
    assert(gaSelector.selectSolution(None, Nil, List(isEven), Nil).isEmpty)
    assert(new RandomSelector[Int].selectSolution(None, candidates, List(isEven, isOdd), Nil).isEmpty)
    assert(ratings.get == 0, "individuals were rated although there was nobody to choose from")
    assert(crossover.calls == 0, "children were bred although there was nobody to breed from")
  }

  it should "still work with no constraints, or with constraints that every candidate satisfies" in {
    for (constraints <- List(Nil, List[Int => Boolean](_ => true), List[Int => Boolean](_ > 0, _ < 100))) {
      val crossover = new CountingCrossover
      val ratings = new AtomicInteger(0)
      val gaSelector = selector(fitnessFunction = { n => ratings.incrementAndGet(); n.toDouble }, geneticOperator = crossover)

      val best = gaSelector.selectSolution(None, candidates, constraints, Nil)

      // Every child is accepted at the first attempt: the same number of children and ratings as without constraints
      assert(best.exists(_ > candidates.max + 1), s"$best did not evolve past ${candidates.max + 1} with $constraints")
      assert(crossover.calls == 30 * (candidates.size - (candidates.size * gaSelector.survivalRate).toInt))
      assert(ratings.get == candidates.size + crossover.calls)
    }
  }

  "breed" should "breed again when a child fails the constraints" in {
    // Every other child is out of bounds
    val calls = new AtomicInteger(0)
    val everyOtherChildFails = new GeneticOperator[Int] {
      override def crossOver(e1: Int, e2: Int): Int = if (calls.incrementAndGet() % 2 == 1) 99 else 5
    }
    val gaSelector = selector(geneticOperator = everyOtherChildFails)
    val parents = (1 to 10).toList.map(n => (n, n.toDouble))

    val children = gaSelector.breed(parents, 6, List(_ <= 20))

    assert(children == List.fill(6)(5))
    assert(calls.get == 12)
  }

  it should "give up after a bounded number of attempts and use a copy of a parent when every child fails" in {
    val calls = new AtomicInteger(0)
    val everyChildFails = new GeneticOperator[Int] {
      override def crossOver(e1: Int, e2: Int): Int = { calls.incrementAndGet(); 99 }
    }
    val gaSelector = selector(geneticOperator = everyChildFails)
    val parents = (1 to 10).toList.map(n => (n, n.toDouble))

    val children = gaSelector.breed(parents, 6, List(_ <= 10))

    assert(children.size == 6)
    assert(children.forall(c => parents.exists(_._1 == c)), s"$children has a child that is not a copy of a parent")
    assert(calls.get == 6 * GASelector.MaxBreedingAttempts)
  }

  it should "not look at the constraints when there are none" in {
    val crossover = new CountingCrossover
    val gaSelector = selector(geneticOperator = crossover)

    gaSelector.breed((1 to 10).toList.map(n => (n, n.toDouble)), 6)

    assert(crossover.calls == 6)
  }

  // The same, with the real crossover operator and real musical cases

  private val NoteLengthNS: BigInt = SECONDS.toNanos(1)

  private def phraseOf(pitches: Seq[Int]): Phrase = Phrase().withMusicalElements(
    pitches.zipWithIndex.map { case (pitch, idx) =>
      Note(midiPitch = pitch, durationNS = NoteLengthNS, startTimeNS = NoteLengthNS * idx)
    })

  private def playsPitch(pitch: Int)(musicalCase: MusicalCase): Boolean =
    musicalCase.phrase.musicalElements.exists { case n: Note => n.midiPitch == pitch; case _ => false }

  private def musicalCaseSelector(geneticOperator: GeneticOperator[MusicalCase]) =
    new GASelector[MusicalCase, MusicalCase](
      fitnessFunction = FitnessFunction.ignoringContext(_ => 1.0),
      stoppingCriteria = IterationLimitReached(10),
      chromosomeGenerator = new ChromosomeGenerator[MusicalCase, MusicalCase] {
        override def createChromosome(elem: MusicalCase): MusicalCase = elem
        override def fromChromosome(chromosome: MusicalCase): MusicalCase = chromosome
      },
      geneticOperator = geneticOperator)

  "A GASelector with the musical case crossover" should "never select a phrase for another instrument class" in {
    // This is the constraint of AIMusician.getCompositionConstraints, for a piano
    val sameClassAsPiano: MusicalCase => Boolean = _.instrumentType.sameInstrumentClass(PIANO(1))
    val pianoCases = (0 until 6).map(i => MusicalCase(PIANO(1 + i), Jazz, phraseOf(Seq.tabulate(5)(60 + i + _))))
    val bassCases = (0 until 6).map(i => MusicalCase(BASS(33 + i), Jazz, phraseOf(Seq.tabulate(5)(40 + i + _))))
    val gaSelector = musicalCaseSelector(new MusicalCaseCrossoverOperator)

    val results = List.fill(30) {
      gaSelector.selectSolution(None, Random.shuffle((pianoCases ++ bassCases).toList), List(sameClassAsPiano), Nil)
    }

    assert(results.forall(_.exists(sameClassAsPiano)), s"${results.flatten.map(_.instrumentType)} has another instrument class")
  }

  it should "never select a child that fails the constraint, although the crossover makes some" in {
    // Segments of the parents are alternated, so a child loses the pitch when it was in a segment that it did not take.
    // Phrases that play 63 do so at a different place each; the others never do
    val plays63: MusicalCase => Boolean = playsPitch(63)
    val with63 = (0 until 5).map { at =>
      MusicalCase(PIANO(1), Jazz, phraseOf(Seq.tabulate(5)(i => if (i == at) 63 else 80 + i)))
    }
    val without63 = (0 until 4).map(i => MusicalCase(PIANO(1), Jazz, phraseOf(Seq.tabulate(5)(80 + i + _))))
    val childrenThatFail = new AtomicInteger(0)
    val realCrossover = new MusicalCaseCrossoverOperator
    val gaSelector = musicalCaseSelector(new GeneticOperator[MusicalCase] {
      override def crossOver(c1: MusicalCase, c2: MusicalCase): MusicalCase = {
        val child = realCrossover.crossOver(c1, c2)
        if (!plays63(child)) childrenThatFail.incrementAndGet()
        child
      }
    })

    val results = List.fill(30) {
      gaSelector.selectSolution(None, Random.shuffle((with63 ++ without63).toList), List(plays63), Nil)
    }

    assert(childrenThatFail.get > 0, "the crossover never made a child that fails the constraint: nothing was checked")
    assert(results.forall(_.exists(plays63)), "the solution does not play 63")
  }
}
