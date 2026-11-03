package representation

/**
 * A time signature such as 3/4 or 6/8: `numerator` beats to a bar, each beat a `1/denominator` note.
 *
 * The denominator is a power of two (MIDI cannot store anything else).
 */
final case class TimeSignature(numerator: Int, denominator: Int) {
  require(numerator > 0, s"A time signature needs at least one beat per bar: $this")
  require(denominator > 0 && Integer.bitCount(denominator) == 1, s"The denominator must be a power of two: $this")

  /**
   * Length of one bar in beats, where a beat is a quarter note, like everywhere else in improv (tempos are in quarter
   * notes per minute and jMusic's rhythm values are in quarter notes): 3/4 is 3, 6/8 is 3, 2/2 is 4, 5/16 is 1.25.
   */
  val barLengthBeats: BigDecimal = BigDecimal(numerator) * 4 / denominator

  override def toString: String = s"$numerator/$denominator"
}

object TimeSignature {
  /** What MIDI assumes before the first time signature event of a file. */
  val Default: TimeSignature = TimeSignature(4, 4)
}

/** The time signature that holds from `startBeat` (in quarter notes from the start of the piece) up to the next change. */
final case class TimeSignatureChange(startBeat: BigDecimal, signature: TimeSignature)
