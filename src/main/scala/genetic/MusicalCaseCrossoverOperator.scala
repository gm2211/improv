package genetic

import cbr.MusicalCase
import representation.Phrase
import training.segmentation.PhraseSegmenter
import utils.NumericUtils
import utils.collections.CollectionUtils

class MusicalCaseCrossoverOperator extends GeneticOperator[MusicalCase] {
  override def crossOver(c1: MusicalCase, c2: MusicalCase): MusicalCase = {
    val parent1 = Phrase.mergePhrases(c1.phrase)
    val parent2 = Phrase.mergePhrases(c2.phrase)
    var child = Phrase()

    val duration = NumericUtils.min(parent1.getDurationNS, parent2.getDurationNS)

    val segmenter = PhraseSegmenter.getDefault(duration / 5)
    var parents = (segmenter.segment(parent1), segmenter.segment(parent2))
    val minNumSegments = scala.math.min(parents._1.size, parents._2.size)

    for (i <- 1 until minNumSegments) {
      child = child.withMusicalElements(parents._1(i): Iterable[representation.MusicalElement])
      parents = parents.swap
    }
    val childInstrument = CollectionUtils.chooseRandom(List(c1.instrumentType, c2.instrumentType)).get
    val childGenre = CollectionUtils.chooseRandom(List(c1.genre, c2.genre)).get
    MusicalCase(childInstrument, childGenre, child)
  }
}
