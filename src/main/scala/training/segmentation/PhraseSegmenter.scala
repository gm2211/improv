package training.segmentation

import representation.{Phrase, TimeSignatureChange}

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.SECONDS

object PhraseSegmenter {
  val DEFAULT_SUB_PHRASE_LENGTH_NS: BigInt = SECONDS.toNanos(5)

  /** Bars in a phrase: 2 bars of 4/4 are 8 beats, close to the median of the 10 beats that the old 5 second cut gave. */
  val DEFAULT_BARS_PER_PHRASE = 2

  /**
   * Cuts every `splitEveryNS` nanoseconds (5 seconds by default), whatever the music is doing. This is not how the case
   * library cuts phrases any more (see [[byBars]]); [[genetic.MusicalCaseCrossoverOperator]] uses it to cut its parents
   * into segments of a fraction of their length.
   */
  def getDefault(splitEveryNS: BigInt = DEFAULT_SUB_PHRASE_LENGTH_NS): PhraseSegmenter =
    new SimpleSplitTimesFinder(splitEveryNS) with PhraseSegmenter

  /**
   * Cuts a piece every `barsPerPhrase` bars, starting at bar 1 (time 0), from its time signature events and its tempo;
   * this is the cut that the case library and the corpus exporter use (`JMusicMIDIParser(filename)`, #27).
   * See [[BarSplitTimesFinder]].
   */
  def byBars(
      timeSignatures: Seq[TimeSignatureChange],
      tempoBPM: Double,
      barsPerPhrase: Int = DEFAULT_BARS_PER_PHRASE): PhraseSegmenter =
    new BarSplitTimesFinder(timeSignatures, tempoBPM, barsPerPhrase) with PhraseSegmenter
}

trait PhraseSegmenter {
  this: SplitTimesFinder =>

  def segment(phrase: Phrase): List[Phrase] = {
    var curPhrase = Option(phrase)
    val phrases = ListBuffer[Phrase]()
    val splitTimes = getSplitTimes(phrase)

    if (splitTimes.isEmpty) {
      return List(phrase)
    }

    for (curTime <- splitTimes if curPhrase.isDefined) {
      val (newPhrase, rest) = Phrase.split(curPhrase.get, curTime)
      newPhrase.foreach(phrases.+=)
      curPhrase = rest
    }
    phrases.toList
  }
}
