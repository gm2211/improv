package demos

import training.ann.{NeuralNetworks, ANNTrainingData}
import utils.collections.CollectionUtils

import scala.util.Random

object DemoDummyANN {
  def main(args: Array[String]): Unit = run()

  def run(): Unit = {
    val dimensions = 3
    val doubles: Array[Double] = Array.fill[Double](dimensions)(Random.nextDouble() * 10)
    var data = new ANNTrainingData(dimensions, 1)
      .addDataPoint(doubles, Array(5.3))
    (1 to 10).foreach { i =>
      val input = Array.fill(dimensions)(Random.nextDouble())
      data.addDataPoint(input, Array(input.toList.sum))
    }
    data = data.normalised
    println(data.toCSV)
    data.saveCSV("/tmp/dummy.csv")
    val net = NeuralNetworks.createNeurophDefault(List(data))
    net.setInput(doubles*)
    net.calculate()
    CollectionUtils.print(net.getOutput)
    println("done")

  }
}
