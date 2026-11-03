package cbr.description.features.extractors.phrase

import cbr.description.features.Feature
import cbr.description.features.extractors.SingleFeatureExtractor
import representation.{Note, Phrase}

class BasicPitchHistogramExtractor extends SingleFeatureExtractor[Phrase] {
  override def extractFeature(phrase: Phrase): Feature[Phrase] =
    Feature.from(Phrase.computePitchHistogram(phrase))

  override val featureSize: Int = Note.MAX_MIDI_PITCH + 1 // 0 to MAX_MIDI_PITCH
}
