package representation

import representation.KeySignature.{Major, Minor}

/** The key signature that holds from `startBeat` (in quarter notes from the start of the piece) up to the next change. */
final case class KeySignatureChange(startBeat: BigDecimal, keySignature: KeySignature)

object KeySignatureChange {
  /** What a piece is in before its first key signature event, and throughout when it has none: C major, as for MIDI. */
  val Default: KeySignature = Phrase.DEFAULT_KEY_SIGNATURE

  /** The key of a MIDI key signature event: `sharps` sharps (negative: flats), major or minor. */
  def fromMidi(sharps: Int, minor: Boolean): KeySignature =
    KeySignature(flats = scala.math.max(-sharps, 0), sharps = scala.math.max(sharps, 0), if (minor) Minor else Major)

  /**
   * The stretches of a piece with one key each, ordered by start, always beginning at beat 0 and never two in a row
   * with the same key: [[Default]] holds from the start until the first change, of several changes at the same beat
   * the last one holds, and a change to the key that already holds is no change.
   */
  def sections(changes: Seq[KeySignatureChange]): Vector[KeySignatureChange] = {
    val ordered = changes.filter(_.startBeat >= 0).sortBy(_.startBeat) // stable: file order within a beat
    val lastPerBeat = ordered.foldLeft(Vector.empty[KeySignatureChange]) { (kept, change) =>
      if (kept.lastOption.exists(_.startBeat == change.startBeat)) kept.init :+ change else kept :+ change
    }
    val fromTheStart =
      if (lastPerBeat.headOption.exists(_.startBeat == 0)) lastPerBeat
      else KeySignatureChange(BigDecimal(0), Default) +: lastPerBeat
    fromTheStart.foldLeft(Vector.empty[KeySignatureChange]) { (kept, change) =>
      if (kept.lastOption.exists(_.keySignature == change.keySignature)) kept else kept :+ change
    }
  }

  /** The key in force at `beat`. */
  def at(changes: Seq[KeySignatureChange], beat: BigDecimal): KeySignature =
    sections(changes).takeWhile(_.startBeat <= beat).lastOption.map(_.keySignature).getOrElse(Default)

  /**
   * The key that holds for the longest time between `fromBeat` and `untilBeat`, summed over all the stretches with that
   * key; of keys that last as long, the one that comes first. A window of no length gets the key in force at its start.
   */
  def longestIn(changes: Seq[KeySignatureChange], fromBeat: BigDecimal, untilBeat: BigDecimal): KeySignature = {
    if (untilBeat <= fromBeat) return at(changes, fromBeat)

    val stretches = sections(changes)
    val ends = stretches.drop(1).map(_.startBeat) :+ untilBeat.max(stretches.last.startBeat)
    val lengths = stretches.zip(ends).flatMap { case (section, end) =>
      val length = end.min(untilBeat) - section.startBeat.max(fromBeat)
      Option.when(length > 0)(section.keySignature -> length)
    }
    // in order of appearance, so that maxBy (which keeps the first of equal values) picks the first key of a tie
    val byKey = lengths.foldLeft(Vector.empty[(KeySignature, BigDecimal)]) { case (sums, (key, length)) =>
      sums.indexWhere(_._1 == key) match {
        case -1 => sums :+ (key -> length)
        case index => sums.updated(index, key -> (sums(index)._2 + length))
      }
    }
    byKey.maxByOption(_._2).map(_._1).getOrElse(at(changes, fromBeat))
  }
}
