package representation

import scala.concurrent.duration.{MILLISECONDS, NANOSECONDS, TimeUnit}

object MusicalElement {
  val DEFAULT_TEMPO_BPM = 120.0

  /**
   * How close to one of its ends a cut has to fall for [[split]] to leave the element whole: 1 microsecond. The noise
   * is a nanosecond or two (jMusic's times are doubles that [[fromBPM]] truncates to whole nanoseconds; in the 76
   * training files the notes it leaves at a cut are 1 ns or 2 ns long), and the shortest real notes there are about
   * 100 microseconds (a MIDI tick or two), so any value in between would do.
   */
  val SPLIT_TOLERANCE_NS: BigInt = 1000

  private def tempoInBeatsPerNanoSec(tempoBPM: BigDecimal): BigDecimal = (tempoBPM / 60) / 1000000000

  def fromBPM(
      beats: BigDecimal,
      tempoBPM: BigDecimal = DEFAULT_TEMPO_BPM,
      resultTimeUnit: TimeUnit = NANOSECONDS): BigInt = {
    resultTimeUnit.convert((beats / tempoInBeatsPerNanoSec(tempoBPM)).toLong, NANOSECONDS)
  }

  def toBPM(
      time: BigInt,
      tempoBPM: BigDecimal = DEFAULT_TEMPO_BPM,
      timeUnit: TimeUnit = NANOSECONDS): BigDecimal = {
    timeUnit.toNanos(time.toLong) * tempoInBeatsPerNanoSec(tempoBPM)
  }

  def isActive(timeNS: BigInt, elem: MusicalElement): Boolean =
    elem.getStartTimeNS <= timeNS && timeNS < elem.getStartTimeNS + elem.getDurationNS

  def resizeIfActive(newStartTimeNS: BigInt, elem: MusicalElement): Option[MusicalElement] = {
    if (MusicalElement.isActive(newStartTimeNS, elem)) fromNewStartTimeToPreviousEndTime(elem, newStartTimeNS)
    else None
  }

  def fromNewStartTimeToPreviousEndTime(elem: MusicalElement, newStartTimeNS: BigInt): Option[MusicalElement] = {
    require(newStartTimeNS > 0)
    val newDuration: BigInt = elem.getEndTimeNS - newStartTimeNS
    Option.when(newDuration > 0) {
      elem
        .withStartTime(newStartTimeNS)
        .withDuration(newDuration)
    }
  }

  def fromPreviousStartTimeToNewEndTime(elem: MusicalElement, newEndTimeNS: BigInt): Option[MusicalElement] = {
    require(newEndTimeNS > 0)
    val newDuration = newEndTimeNS - elem.getStartTimeNS
    Option.when(newDuration > 0)(elem.withDuration(newDuration))
  }

  /**
   * Cuts `elem` at `splitTimeNS` into what is before the cut and what is after it.
   *
   * A cut that falls less than `toleranceNS` from an end of the element moves to that end: the element is not cut and
   * goes whole to the side it lies on, unchanged. Without this, a note that starts a nanosecond before the cut, which
   * is what jMusic's rounding noise makes of a note that starts on it (#33), would leave a 1 ns note at the end of the
   * left side and shorten the note on the right side. An element that is shorter than twice the tolerance is cut as
   * before, and `toleranceNS = 0` cuts exactly where it is told to.
   */
  def split(
      elem: MusicalElement,
      splitTimeNS: BigInt,
      toleranceNS: BigInt = SPLIT_TOLERANCE_NS): (Option[MusicalElement], Option[MusicalElement]) = {
    var left: Option[MusicalElement] = None
    var right: Option[MusicalElement] = None

    val before = splitTimeNS - elem.getStartTimeNS // the part that would be left of the cut
    val after = elem.getEndTimeNS - splitTimeNS // the part that would be right of the cut

    if (before > 0 && before < toleranceNS && after >= toleranceNS) {
      right = Some(elem)
    } else if (after > 0 && after < toleranceNS && before >= toleranceNS) {
      left = Some(elem)
    } else if (elem.getStartTimeNS <= splitTimeNS && splitTimeNS <= elem.getEndTimeNS) {
      left = fromPreviousStartTimeToNewEndTime(elem, splitTimeNS)
      right = fromNewStartTimeToPreviousEndTime(elem, splitTimeNS)
    } else if (splitTimeNS > elem.getEndTimeNS) {
      left = Some(elem)
    } else if (splitTimeNS < elem.getStartTimeNS) {
      right = Some(elem)
    }
    (left, right)
  }
}

trait MusicalElement {
  def withStartTime(startTime: BigInt, timeUnit: TimeUnit = NANOSECONDS): MusicalElement

  def withStartTimeBPM(startTimeBPM: BigDecimal, tempoBPM: Double): MusicalElement =
    withStartTime(MusicalElement.fromBPM(startTimeBPM, tempoBPM))

  def withEndTime(endTime: BigInt, timeUnit: TimeUnit = NANOSECONDS): MusicalElement = {
    val duration = endTime - getStartTime(timeUnit)
    require(duration >= 0)
    withDuration(duration, timeUnit)
  }

  def withEndTimeBPM(endTimeBPM: BigDecimal, tempoBPM: Double): MusicalElement =
    withEndTime(MusicalElement.fromBPM(endTimeBPM, tempoBPM))

  def withDuration(duration: BigInt, timeUnit: TimeUnit = NANOSECONDS): MusicalElement

  def withDurationBPM(durationBPM: BigDecimal, tempoBPM: Double): MusicalElement =
    withDuration(MusicalElement.fromBPM(durationBPM, tempoBPM))

  def getDurationBPM(tempoBPM: BigDecimal): BigDecimal = MusicalElement.toBPM(getDurationNS, tempoBPM = tempoBPM)

  def getDurationNS: BigInt = getDuration(NANOSECONDS)

  def getDuration(timeUnit: TimeUnit): BigInt

  def getStartTimeBPM(tempoBPM: BigDecimal): BigDecimal = MusicalElement.toBPM(getStartTimeNS, tempoBPM = tempoBPM)

  def getStartTimeNS: BigInt = getStartTime(NANOSECONDS)

  def getStartTime(timeUnit: TimeUnit): BigInt

  def getEndTimeNS: BigInt = getStartTimeNS + getDurationNS

  override def toString: String = s"${getClass.getTypeName} => startTime: ${getStartTime(MILLISECONDS)}; duration: ${getDuration(MILLISECONDS)} "
}
