package training.ann

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NeuralNetworksTest extends AnyFlatSpec with Matchers {

  "The default neural network" should "load from NN.nnet and compute an output" in {
    val network = NeuralNetworks.load().get

    val inputs = network.getInputsCount
    val outputs = network.getOutputsCount
    inputs should be > 0

    network.setInput(Array.fill(inputs)(0.5)*)
    network.calculate()
    val output = network.getOutput

    output.length shouldBe outputs
    all(output.toSeq) should (be >= 0.0 and be <= 1.0)
  }
}
