package demos

import training.ann.ANNTrainingData
import utils.IOUtils

object DemoMergeTrainingData {
  def main(args: Array[String]): Unit = {
    val filenames = IOUtils.filesInDir("~/Desktop/DataAnn").get
    val annData = ANNTrainingData.merge(filenames.map(f => ANNTrainingData.loadFromCSV(f).get))
    annData.normalised.saveCSV("/Users/gm2211/Desktop/trainingData")
  }
}
