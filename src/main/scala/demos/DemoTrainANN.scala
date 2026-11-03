package demos

import cbr.description.PhraseDescriptionCreators
import designPatterns.observer.{EventNotification, Observer}
import instruments.JFugueUtils
import midi.MIDIPlayer
import storage.MapDBMapStore
import training.MusicalCaseExtractors
import training.ann.{ANNTrainingData, NeuralNetworks}
import utils.collections.CollectionUtils

object DemoTrainANN extends Observer {
  def main(args: Array[String]): Unit = run()

  def run(): Unit = {
    val db = ANNTrainingData.loadDefaultDB
    val trainingData = db.keySet().map(db.get(_).get)

    val neuralNet = NeuralNetworks.createNeurophDefault(trainingData.toList)

    val songFile = db.keySet().head
    val extractor = MusicalCaseExtractors.getDefault()

    val cases = extractor.getCases(songFile).map(_._2)
    val descriptionCreator = PhraseDescriptionCreators.getDefault
    val player = new MIDIPlayer
    player.addObserver(this)

    cases.foreach{ musCase =>
      player.play(JFugueUtils.toSequence(musCase.phrase, musCase.instrumentType))

      this.synchronized{
        while(player.playing) {
          wait()
        }
      }

      val description = descriptionCreator.createCaseDescription(musCase)
      neuralNet.setInput(description.getSignature*)

      neuralNet.calculate()
      val rating = neuralNet.getOutput

      print("The system rated this phrase: ")
      CollectionUtils.print(rating)
      scala.io.StdIn.readLine("Press key to continue")
    }

  }

  override def notify(eventNotification: EventNotification): Unit =
    this.synchronized(notifyAll())
}
