package genetic

import cbr.MusicalCase
import cbr.description.{DefaultPhraseDescriptionCreator, DescriptionCreator}
import org.neuroph.core.NeuralNetwork
import training.ann.NeuralNetworks

object ANNFitnessFunction {
  def getDefault = {
    new ANNFitnessFunction(NeuralNetworks.load().get)
  }
}

class ANNFitnessFunction(
    neuralNet: NeuralNetwork[?],
    descriptionCreator: DescriptionCreator[MusicalCase] = new DefaultPhraseDescriptionCreator)
      extends FitnessFunction[MusicalCase] {

  override def apply(musicalCase: MusicalCase): Double = {
    val input = descriptionCreator.createCaseDescription(musicalCase).getSignature
    neuralNet.setInput(input*)
    neuralNet.calculate()
    neuralNet.getOutput()(0)
  }
}
