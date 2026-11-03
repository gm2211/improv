package training.ann

import java.io.File

import org.neuroph.core.NeuralNetwork
import org.neuroph.core.events.{LearningEvent, LearningEventListener, NeuralNetworkEvent, NeuralNetworkEventListener}
import org.neuroph.core.learning.stop.SmallErrorChangeStop
import org.neuroph.nnet.MultiLayerPerceptron
import org.neuroph.nnet.learning.{MomentumBackpropagation, BackPropagation}
import org.neuroph.util.TransferFunctionType
import utils.IOUtils

import scala.util.Try

object NeuralNetworks {
  val DEFAULT_ANN_PATH: String = IOUtils.getResourcePath("knowledgeBase/NN.nnet")

  def createNeurophDefault(data: List[ANNTrainingData]): NeuralNetwork[BackPropagation] = {
    val mergedTrainingData = ANNTrainingData.merge(data)
    val neuronsInInputLayer = mergedTrainingData.inputDimensions
    val neuronsInHiddenLayer1 = 20
    val neuronsInHiddenLayer2 = 20
    val neuronsInOutputLayer = mergedTrainingData.outputDimensions

    val neuralNet = new MultiLayerPerceptron(
      TransferFunctionType.SIGMOID,
      neuronsInInputLayer,
      neuronsInHiddenLayer1,
      neuronsInHiddenLayer2,
      neuronsInOutputLayer)

    val rule = new FasterMomentumBackPropagation
//    rule.addListener(new LearningEventListener {
//      override def handleLearningEvent(event: LearningEvent): Unit = println(rule.getTotalNetworkError)
//    })

    neuralNet.learn(mergedTrainingData.normalised, rule)
    neuralNet.save(DEFAULT_ANN_PATH)

    neuralNet
  }

  def load(filename: String = DEFAULT_ANN_PATH): Try[NeuralNetwork[?]] =
    Try(NeuralNetwork.createFromFile(new File(filename)))


  class FasterMomentumBackPropagation extends MomentumBackpropagation {
    stopConditions.add(new SmallErrorChangeStop(this))
    setMinErrorChangeIterationsLimit(100000)
  }
}
