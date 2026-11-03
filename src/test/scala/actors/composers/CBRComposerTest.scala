package actors.composers

import java.util.concurrent.ConcurrentLinkedQueue

import cbr.description.{CaseDescription, DefaultPhraseDescriptionCreator, DescriptionCreator}
import cbr.{CaseIndex, MusicalCase}
import genetic.{ChromosomeGenerator, FitnessContext, GASelector, MusicalCaseCrossoverOperator, SolutionSelector}
import instruments.InstrumentType.PIANO
import org.scalatest.flatspec.AnyFlatSpec
import representation.{Jazz, Note, Phrase}

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.MILLISECONDS
import scala.jdk.CollectionConverters.*

class CBRComposerTest extends AnyFlatSpec {

  private def caseOf(pitches: Int*): MusicalCase = {
    val noteDurationNS = MILLISECONDS.toNanos(200)
    val notes = pitches.zipWithIndex.map { case (pitch, idx) =>
      Note(midiPitch = pitch, durationNS = noteDurationNS, startTimeNS = noteDurationNS * idx)
    }
    MusicalCase(PIANO(1), Jazz, Phrase().withMusicalElements(notes))
  }

  /** An index that finds the same solutions for every problem */
  private class FixedCaseIndex(solutions: List[MusicalCase]) extends CaseIndex[MusicalCase] {
    override val descriptionCreator: DescriptionCreator[MusicalCase] = new DefaultPhraseDescriptionCreator
    override def iterator: Iterator[CaseDescription[MusicalCase]] = Iterator.empty
    override def findSolutionsToSimilarProblems(problemDescription: CaseDescription[MusicalCase], k: Int): List[MusicalCase] = solutions
    override def addSolutionToProblem(problemDescription: CaseDescription[MusicalCase], solution: MusicalCase): Unit = ()
    override def removeSolutionsToProblem(problemDescription: CaseDescription[MusicalCase]): Boolean = false
    override def clear(): this.type = this
    override def compact(): this.type = this
  }

  private case class SelectorCall(
      previousSolution: Option[MusicalCase],
      candidates: List[MusicalCase],
      constraints: List[MusicalCase => Boolean],
      phrasesByOthers: Iterable[MusicalCase])

  /** A selector that records what it is asked and always selects the first candidate */
  private class RecordingSelector extends SolutionSelector[MusicalCase] {
    val calls = ListBuffer[SelectorCall]()

    override def selectSolution(
        previousSolution: Option[MusicalCase],
        candidateSolutions: List[MusicalCase],
        constraints: List[MusicalCase => Boolean],
        phrasesByOthers: Iterable[MusicalCase]): Option[MusicalCase] = {
      calls += SelectorCall(previousSolution, candidateSolutions, constraints, phrasesByOthers)
      candidateSolutions.headOption
    }
  }

  private val phrasesByOthers = List(caseOf(60, 62, 64, 65, 67), caseOf(48, 50, 52, 53, 55))
  private val solutions = List(caseOf(70, 72, 74, 75, 77), caseOf(80, 82, 84, 85, 87))
  private val previousSolution = Some(caseOf(40, 41, 42, 43, 44))
  private val constraints: List[MusicalCase => Boolean] = List(_ => true)

  "The CBRComposer" should "hand the selector the phrases by the others, along with the previous solution" in {
    val selector = new RecordingSelector
    val composer = new CBRComposer(new FixedCaseIndex(solutions), Some(selector))

    composer.compose(previousSolution, phrasesByOthers, constraints)

    assert(selector.calls.size == 1)
    val call = selector.calls.head
    assert(call.phrasesByOthers.toList == phrasesByOthers)
    assert(call.previousSolution == previousSolution)
    assert(call.constraints == constraints)
    assert(call.candidates.distinct == solutions)
  }

  it should "compose the phrase of the solution that the selector selects" in {
    val composer = new CBRComposer(new FixedCaseIndex(solutions), Some(new RecordingSelector))

    assert(composer.compose(previousSolution, phrasesByOthers, constraints).contains(solutions.head.phrase))
  }

  it should "still compose with the random selector it uses by default" in {
    val composer = new CBRComposer(new FixedCaseIndex(solutions))

    val phrases = List.fill(20)(composer.compose(None, phrasesByOthers, Nil))

    assert(phrases.forall(phrase => phrase.exists(solutions.map(_.phrase).contains)))
  }

  it should "get the fitness function of a GASelector to rate every candidate as a response to the others' phrases" in {
    val contexts = new ConcurrentLinkedQueue[FitnessContext[MusicalCase]]()
    val selector = new GASelector[MusicalCase, MusicalCase](
      fitnessFunction = { (candidate, context) => contexts.add(context); 1.0 },
      chromosomeGenerator = new ChromosomeGenerator[MusicalCase, MusicalCase] {
        override def createChromosome(elem: MusicalCase): MusicalCase = elem
        override def fromChromosome(chromosome: MusicalCase): MusicalCase = chromosome
      },
      geneticOperator = new MusicalCaseCrossoverOperator)
    val composer = new CBRComposer(new FixedCaseIndex(solutions), Some(selector))

    val phrase = composer.compose(previousSolution, phrasesByOthers, constraints)

    assert(phrase.isDefined)
    assert(! contexts.isEmpty)
    assert(contexts.asScala.forall(_ == FitnessContext(phrasesByOthers, previousSolution)))
  }
}
