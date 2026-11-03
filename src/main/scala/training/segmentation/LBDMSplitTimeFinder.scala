package training.segmentation

import midi.JMusicParserUtils
import representation._
import utils.ImplicitConversions.toEnhancedIterable
import utils.NumericUtils

import scala.collection.mutable.ListBuffer

object LBDMSplitTimeFinder {
  val DEFAULT_PITCH_WEIGHT = 0.45
  val DEFAULT_IOI_WEIGHT = 0.35
  val DEFAULT_RESTS_WEIGHT = 0.2
}

class LBDMSplitTimeFinder(private val mergePolyphonic: Boolean = true) extends SplitTimesFinder {
  private val pitchWeight = LBDMSplitTimeFinder.DEFAULT_PITCH_WEIGHT
  private val ioiWeight = LBDMSplitTimeFinder.DEFAULT_IOI_WEIGHT
  private val restsWeight = LBDMSplitTimeFinder.DEFAULT_RESTS_WEIGHT

  override def getSplitTimes(originalPhrase: Phrase): Iterable[BigInt] = {
    val phrase = processPolyphonic(originalPhrase)
    val overallProfile = computeProfile(phrase)
    val peaks = NumericUtils.findPeaks(overallProfile)
    val avg = NumericUtils.avg(peaks.map(_._1))
    val notes = phrase.filter { case r: Rest => false; case _ => true }.toList

    val splitTimes = peaks.withFilter(_._1 >= avg).map { case (_, idx) =>
      notes(idx).getEndTimeNS
    }

    var prevSplitTime = BigDecimal(0)
    // TODO: if `mergePolyphonic`, make sure the splitTimes actually correspond to the non-merged phrase
    val relativeSplitTimes = splitTimes.map { splitTime =>
      val relativeSplit = BigDecimal(splitTime) - prevSplitTime
      prevSplitTime = BigDecimal(splitTime)
      BigInt(relativeSplit.toLong)
    }

    relativeSplitTimes
  }

  def computeProfile(phrase: Phrase): List[BigDecimal] = {
    val phrase_ = processPolyphonic(phrase)
    val pitchProfile = NumericUtils.normalise(computePitchProfile(phrase_))
    val ioiProfile = NumericUtils.normalise(computeIOIProfile(phrase_))
    val restsProfile = NumericUtils.normalise(computeRestsProfile(phrase_))

    for (pitchIOIRests <- List(pitchProfile, ioiProfile, restsProfile).zipped.toList) yield {
      pitchWeight * pitchIOIRests.head.getOrElse(BigDecimal(0)) +
        ioiWeight * pitchIOIRests(1).getOrElse(BigDecimal(0)) +
        restsWeight * pitchIOIRests(2).getOrElse(BigDecimal(0))
    }
  }

  def computePitchProfile(phrase: Phrase): List[BigDecimal] = {
    val phrase_ = processPolyphonic(phrase)
    val getPitch = (note: Note) => BigDecimal(note.midiPitch)
    computeProfile(phrase_, _ + _, getPitch, getPitch)
  }

  def computeIOIProfile(phrase: Phrase): List[BigDecimal] = {
    val phrase_ = processPolyphonic(phrase)
    val getStartTime = (note: Note) => BigDecimal(note.startTimeNS)
    computeProfile(phrase_, NumericUtils.min, getStartTime, getStartTime)
  }

  def computeRestsProfile(phrase: Phrase): List[BigDecimal] = {
    val phrase_ = processPolyphonic(phrase)
    val getStartTime = (note: Note) => BigDecimal(note.startTimeNS)
    val getEndTime = (note: Note) => BigDecimal(note.getEndTimeNS)
    computeProfile(phrase_, NumericUtils.min, getEndTime, getStartTime)
  }

  private def computeProfile(
      phrase: Phrase,
      combineFN: (BigDecimal, BigDecimal) => BigDecimal,
      paramFN1: Note => BigDecimal,
      paramFN2: Note => BigDecimal): List[BigDecimal] = {
    require(!phrase.polyphony, "Cannot process polyphonic phrases")
    val intervals = computeIntervals(phrase, combineFN, paramFN1, paramFN2)
    computeProfile(intervals)
  }

  def computeIntervals(
      phrase: Phrase,
      combineFN: (BigDecimal, BigDecimal) => BigDecimal,
      paramFN1: Note => BigDecimal,
      paramFN2: Note => BigDecimal): List[BigDecimal] = {

    def extractValue(elem: MusicalElement, fn: Note => BigDecimal): Option[BigDecimal] = elem match {
      case note: Note =>
        Some(fn(note) + 1)
      case chord: Chord =>
        Some(chord.notes.foldLeft(BigDecimal(0))((acc, note) => combineFN(acc, fn(note) + 1)))
      case _ =>
        None
    }

    var prevValue: Option[BigDecimal] = None
    val intervals = ListBuffer[BigDecimal]()

    for (elem <- phrase) {
      val (nextPrevValue, curValueOpt) = (extractValue(elem, paramFN1), extractValue(elem, paramFN2))

      nextPrevValue.foreach { nextPrevValue =>
        curValueOpt.foreach { curValue =>
          if (prevValue.isDefined) {
            val interval = (prevValue.get - curValue).abs
            intervals += interval
          }

          prevValue = Some(nextPrevValue)
        }
      }
    }
    intervals.toList
  }

  def computeProfile(intervals: List[BigDecimal]) = {
    val profile = ListBuffer[BigDecimal]()
    var prevInterval: Option[BigDecimal] = None
    var prevDegreeOfChange: Option[BigDecimal] = None

    intervals.foreach { interval =>
      if (prevInterval.isDefined) {
        var degreeOfChange: BigDecimal = 0
        val intervalSum = interval + prevInterval.get

        if (intervalSum > 0) {
          degreeOfChange = (interval - prevInterval.get).abs / intervalSum
        }

        if (prevDegreeOfChange.isDefined) {
          profile += interval * (prevDegreeOfChange.get + degreeOfChange)
        }
        prevDegreeOfChange = Some(degreeOfChange)
      }
      prevInterval = Some(interval)
    }

    profile.toList
  }

  private def processPolyphonic(phrase: Phrase): Phrase = {
    if (mergePolyphonic)
      Phrase.mergePhrases(phrase)
    else
      Phrase.getLongestSubPhrase(phrase, List())
  }
}
