package training.segmentation

import representation.{MusicalElement, Phrase, TimeSignature, TimeSignatureChange}

object BarSplitTimesFinder {
  /**
   * The stretches of a piece with one time signature each, ordered by start, always beginning at beat 0 and never two
   * in a row with the same signature:
   *  - 4/4 holds from the start until the first change (MIDI's default), so a piece with no time signature is in 4/4;
   *  - of several changes at the same beat the last one holds;
   *  - a change to the signature that already holds does not start a new bar, so it leaves the bar lines alone.
   */
  def sections(changes: Seq[TimeSignatureChange]): Vector[TimeSignatureChange] = {
    val ordered = changes.filter(_.startBeat >= 0).sortBy(_.startBeat) // stable: file order within a beat
    val lastPerBeat = ordered.foldLeft(Vector.empty[TimeSignatureChange]) { (kept, change) =>
      if (kept.lastOption.exists(_.startBeat == change.startBeat)) kept.init :+ change else kept :+ change
    }
    val fromTheStart =
      if (lastPerBeat.headOption.exists(_.startBeat == 0)) lastPerBeat
      else TimeSignatureChange(BigDecimal(0), TimeSignature.Default) +: lastPerBeat
    fromTheStart.foldLeft(Vector.empty[TimeSignatureChange]) { (kept, change) =>
      if (kept.lastOption.exists(_.signature == change.signature)) kept else kept :+ change
    }
  }
}

/**
 * Cuts a part on bar lines: every `barsPerPhrase` bars, starting at bar 1, which starts at time 0. A pickup bar (an
 * anacrusis) is not looked for: a piece that starts with one is cut that many beats before its real bar lines. The length of a bar follows
 * the time signature, `numerator` beats of `1/denominator` notes, and the beats become time with `tempoBPM`, the one
 * tempo that all the times of the piece were converted with (so a phrase's length in beats does not depend on the
 * tempo, only on the time signature).
 *
 * A time signature change starts a new bar on the spot, which is also where the old signature ends, whether or not
 * that is a bar line of the old one; bars are counted on across changes, so a phrase can straddle one (3 bars of 4/4
 * then 3/4, cut every 2 bars, gives 8, 7, 6, 6, ... beats: the second phrase is a 4/4 bar and a 3/4 bar) and phrases are
 * as long in beats as their bars add up to.
 *
 * Like [[SimpleSplitTimesFinder]] it only cuts where a whole window fits: a part is cut as many times as the cut
 * times that lie at or before its end (the stretch after the last cut is dropped by [[PhraseSegmenter]]), and a part
 * shorter than the first window is not cut and stays one phrase.
 *
 * @param timeSignatures the time signature events of the piece with their start in quarter notes
 *                       (see [[midi.MidiTimeSignature]]); 4/4 holds until the first one, and with none the piece is in 4/4
 * @param tempoBPM quarter notes per minute
 * @param barsPerPhrase bars in a phrase
 */
class BarSplitTimesFinder(
    timeSignatures: Seq[TimeSignatureChange],
    private val tempoBPM: Double,
    private val barsPerPhrase: Int = PhraseSegmenter.DEFAULT_BARS_PER_PHRASE) extends SplitTimesFinder {
  require(barsPerPhrase > 0, s"A phrase needs at least one bar: $barsPerPhrase")

  private val sections = BarSplitTimesFinder.sections(timeSignatures)

  /** The start of every bar, in beats, in order and without end. */
  private def barStarts: Iterator[BigDecimal] =
    sections.indices.iterator.flatMap { index =>
      val section = sections(index)
      val end = sections.lift(index + 1).map(_.startBeat)
      Iterator.iterate(section.startBeat)(_ + section.signature.barLengthBeats).takeWhile(start => end.forall(start < _))
    }

  /** Where the phrases end, in beats: the start of bar `barsPerPhrase + 1`, of bar `2 * barsPerPhrase + 1`, ... */
  private def cutBeats: Iterator[BigDecimal] =
    barStarts.zipWithIndex.collect { case (start, bar) if bar > 0 && bar % barsPerPhrase == 0 => start }

  /** Each cut relative to the previous one, as [[PhraseSegmenter]] splits what is left after every cut. */
  override def getSplitTimes(phrase: Phrase): Iterable[BigInt] = {
    if (!(tempoBPM > 0)) return Nil

    val end = phrase.getEndTimeNS
    val cuts = cutBeats.map(MusicalElement.fromBPM(_, tempoBPM)).takeWhile(_ <= end).toList
    (BigInt(0) :: cuts).zip(cuts).map { case (previous, cut) => cut - previous }
  }
}
