package representation

import scala.concurrent.duration.{MILLISECONDS, NANOSECONDS, TimeUnit}

object MusicalElement {
  val DEFAULT_TEMPO_BPM = 120.0
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

  def split(elem: MusicalElement, splitTimeNS: BigInt): (Option[MusicalElement], Option[MusicalElement]) = {
    var left: Option[MusicalElement] = None
    var right: Option[MusicalElement] = None

    if (elem.getStartTimeNS <= splitTimeNS && splitTimeNS <= elem.getEndTimeNS) {
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
