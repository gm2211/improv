package cbr.description.features.extractors.phrase

import cbr.description.features.Feature
import cbr.description.features.extractors.SingleFeatureExtractor
import representation.{Note, Phrase}

class AverageNoteDurationExtractor extends SingleFeatureExtractor[Phrase] {
  override def extractFeature(phrase: Phrase): Feature[Phrase] = {
    var noteCount = 0
    var noteDuration = 0.0
    for (elem <- phrase) {
      elem match {
        case note: Note =>
          noteCount += 1
          noteDuration += note.getDurationBPM(phrase.tempoBPM).toDouble
        case _ =>
      }
    }
    Feature.from[Phrase](Array(noteDuration / noteCount))
  }

  override val featureSize: Int = 1

}
