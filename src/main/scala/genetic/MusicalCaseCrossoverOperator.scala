package genetic

import cbr.MusicalCase
import representation.Phrase
import training.segmentation.PhraseSegmenter
import utils.NumericUtils
import utils.collections.CollectionUtils

import scala.concurrent.duration.NANOSECONDS

object MusicalCaseCrossoverOperator {
  /**
   * Number of segments that the shorter of the two parents is cut into
   */
  val SegmentsInShorterParent = 5
}

/**
 * Builds a child by alternating consecutive, equally long segments of its two parents, starting with the first
 * segment of the first parent. The segments are laid out one after the other, so the child is about as long as the
 * shorter parent.
 *
 * Note times are nanoseconds, and that is what is heard, so the segments keep their times whatever the tempo of the
 * parent they come from. The child's `tempoBPM`, which only says how to read those times back as beats, is the tempo of
 * the first parent (the one whose segment comes first). When the parents have different tempos, the beats of the
 * second parent's segments are then read at the first parent's tempo (a segment of 4 beats at 90 BPM is 5.33 beats
 * of a child at 120).
 */
class MusicalCaseCrossoverOperator extends GeneticOperator[MusicalCase] {
  override def crossOver(c1: MusicalCase, c2: MusicalCase): MusicalCase = {
    val parent1 = Phrase.mergePhrases(c1.phrase)
    val parent2 = Phrase.mergePhrases(c2.phrase)
    val child = splice(parent1, parent2)

    val childInstrument = CollectionUtils.chooseRandom(List(c1.instrumentType, c2.instrumentType)).get
    val childGenre = CollectionUtils.chooseRandom(List(c1.genre, c2.genre)).get
    MusicalCase(childInstrument, childGenre, child)
  }

  private def splice(parent1: Phrase, parent2: Phrase): Phrase = {
    val duration = NumericUtils.min(parent1.getDurationNS, parent2.getDurationNS)
    val segmentLength = duration / MusicalCaseCrossoverOperator.SegmentsInShorterParent

    if (segmentLength <= 0) {
      // Too short (or empty) to cut into segments
      parent1
    } else {
      val segmenter = PhraseSegmenter.getDefault(segmentLength)
      var parents = (segmenter.segment(parent1), segmenter.segment(parent2))
      val minNumSegments = scala.math.min(parents._1.size, parents._2.size)
      var child = new Phrase(tempoBPM = parent1.tempoBPM)

      for (i <- 0 until minNumSegments) {
        child = append(child, parents._1(i))
        parents = parents.swap
      }
      child
    }
  }

  /**
   * Adds the segment (which starts at time zero) at the end of the phrase
   */
  private def append(phrase: Phrase, segment: Phrase): Phrase = {
    val shiftedSegment = if (phrase.isEmpty) segment else segment.withStartTime(phrase.getEndTimeNS, NANOSECONDS)
    phrase.withMusicalElements(phrase.musicalElements ++ shiftedSegment.musicalElements)
  }
}
