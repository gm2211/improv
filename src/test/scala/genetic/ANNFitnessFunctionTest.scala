package genetic

import cbr.MusicalCase
import cbr.description.{CaseDescription, DefaultPhraseDescriptionCreator}
import instruments.InstrumentType.PIANO
import org.neuroph.nnet.MultiLayerPerceptron
import org.scalatest.flatspec.AnyFlatSpec
import representation.{Jazz, Note, Phrase}

class ANNFitnessFunctionTest extends AnyFlatSpec {
  private val signature = Array(0.2, 0.7)

  // Describes every case with the same two features, so that the network can be a small one
  private val descriptionCreator = new DefaultPhraseDescriptionCreator {
    override def createCaseDescription(elem: MusicalCase): CaseDescription[MusicalCase] =
      CaseDescription.fromArray[MusicalCase](signature)
  }

  private def caseOf(pitch: Int) = MusicalCase(PIANO(1), Jazz, Phrase().withMusicalElements(Note(midiPitch = pitch)))

  private val network = new MultiLayerPerceptron(2, 3, 1)
  private val fitnessFunction = new ANNFitnessFunction(network, descriptionCreator)
  private val candidate = caseOf(60)

  "The ANNFitnessFunction" should "rate the candidate with the network" in {
    network.setInput(signature*)
    network.calculate()
    val expected = network.getOutput()(0)

    val fitness = fitnessFunction(candidate, FitnessContext())

    assert(fitness == expected)
    assert(fitness > 0.0 && fitness < 1.0)
  }

  it should "rate the candidate the same whatever it is a response to" in {
    val alone = fitnessFunction(candidate, FitnessContext())
    val withOthers = fitnessFunction(candidate, FitnessContext(List(caseOf(48), caseOf(72)), None))
    val withPrevious = fitnessFunction(candidate, FitnessContext(Nil, Some(caseOf(55))))
    val withBoth = fitnessFunction(candidate, FitnessContext(List(caseOf(48)), Some(caseOf(55))))

    assert(withOthers == alone)
    assert(withPrevious == alone)
    assert(withBoth == alone)
  }

  it should "be usable as the fitness function of a GASelector" in {
    val selector = new GASelector[MusicalCase, MusicalCase](
      fitnessFunction,
      chromosomeGenerator = new ChromosomeGenerator[MusicalCase, MusicalCase] {
        override def createChromosome(elem: MusicalCase): MusicalCase = elem
        override def fromChromosome(chromosome: MusicalCase): MusicalCase = chromosome
      },
      geneticOperator = new GeneticOperator[MusicalCase] {
        override def crossOver(e1: MusicalCase, e2: MusicalCase): MusicalCase = e1
      })

    val selected = selector.selectSolution(None, List(caseOf(60), caseOf(61)), Nil, List(caseOf(48)))

    assert(selected.isDefined)
  }
}
