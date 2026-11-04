package training

import cbr.MusicalCase
import instruments.InstrumentType.InstrumentType
import midi.{MIDIParser, JMusicMIDIParser, MIDIParserFactory}
import representation.{Rest, Phrase}
import utils.ImplicitConversions.toEnhancedIterable
import utils.functional.FunctionalUtils

import scala.collection.mutable.ListBuffer

object MusicalCaseExtractors {
  def getDefault(parserFactory: (String) => MIDIParser = JMusicMIDIParser) =
    new DefaultMusicalCaseExtractor(parserFactory)
}
/**
 * Extracts cases from a midi file
 */
class DefaultMusicalCaseExtractor(private val parserFactory: (String) => MIDIParser = JMusicMIDIParser) extends CaseExtractor[MusicalCase] {
  override def getCases(filename: String): List[(MusicalCase, MusicalCase)] = {
    val parser = parserFactory.apply(filename)
    val instrParts: List[(InstrumentType, List[Phrase])] = parser.getPartIndexByInstrument.toList.flatMap {
      case (instrument, partIndices) =>
        partIndices.map(idx => (instrument, parser.getMultiVoicePhrases(idx))).toList
    }
    getAllCases(instrParts)
  }

  def getAllCases(instrParts: List[(InstrumentType, List[Phrase])]) = {
    val indices = instrParts.indices
    val cases = ListBuffer[(MusicalCase, MusicalCase)]()

    for (
          (instr, part) <- instrParts;
          (phrase: Phrase, phraseIdx) <- part.zipWithIndex;
          otherPartIdx <- indices
        ) {
      val (otherInstr, otherPart) = instrParts(otherPartIdx)
      val successorPhrase = Option.when(otherPart.inBounds(phraseIdx + 1))(otherPart(phraseIdx + 1))
      successorPhrase.foreach{ successor =>
        if (getFilters(phrase) && getFilters(successor)) {
          cases += ( (MusicalCase(instr, phrase = phrase),
                      MusicalCase(otherInstr, phrase = successor)) )
        }
      }
    }
    cases.toList
  }

  /**
   * Whether a phrase may enter the case library: it is not silent throughout, it does not start or end in silence, and
   * it is silent for less than 2/3 of its length. See [[DefaultMusicalCaseExtractor.isMostlyMusic]].
   */
  def getFilters = (phrase: Phrase) => DefaultMusicalCaseExtractor.isMostlyMusic(phrase)
}

object DefaultMusicalCaseExtractor {
  /** Silence shorter than this is rounding noise between notes, not a pause: [[Phrase.GAP_TOLERANCE_NS]] (#39). */
  private val TOLERANCE_NS: BigInt = Phrase.GAP_TOLERANCE_NS

  /**
   * A phrase is rejected when it is all rest, starts or ends in silence, or is silent for 2/3 of its length or more.
   *
   * For a monophonic phrase silence is its rests: the first or the last element is a `Rest`, or the rests add up to 2/3
   * of its length. A polyphonic phrase is judged the same way as a whole, i.e. by the time in which none of its voices
   * plays a note (#44): it starts in silence when no voice has a note at its start (one voice that waits for its entry
   * while another plays is not silence), it ends in silence when no voice plays up to its end, and its silence is the
   * time that no voice covers. Before, the head / last / 2/3 checks looked at the elements of a polyphonic phrase, which
   * are its voices and never `Rest`s, so only the "all rest" check applied to it (83% of the library). A polyphonic
   * phrase with a single voice is judged exactly like that voice on its own.
   */
  def isMostlyMusic(phrase: Phrase): Boolean =
    if (phrase.polyphony) isMostlyMusicPolyphonic(phrase) else isMostlyMusicMonophonic(phrase)

  private def isMostlyMusicMonophonic(phrase: Phrase): Boolean = {
    !Phrase.allRest(phrase) &&
    !phrase.head.isInstanceOf[Rest] &&
    !phrase.last.isInstanceOf[Rest] &&
    !(phrase.collect{case r:Rest => r.getDurationNS}.sum >= (2.0/3.0 * phrase.getDurationNS.toLong).toLong)
  }

  private def isMostlyMusicPolyphonic(phrase: Phrase): Boolean = {
    val voices = phrase.musicalElements.collect { case voice: Phrase if voice.nonEmpty => voice }
    val sounding = soundingIntervalsNS(phrase)
    if (voices.isEmpty || sounding.isEmpty) return false

    val start = voices.map(_.getStartTimeNS).min
    val end = voices.map(_.musicalElements.last.getEndTimeNS).max
    val silenceNS = (end - start) - sounding.map { case (from, until) => until - from }.sum

    sounding.head._1 <= start + TOLERANCE_NS &&
    sounding.last._2 >= end - TOLERANCE_NS &&
    BigDecimal(silenceNS) < BigDecimal(end - start) * 2 / 3
  }

  /**
   * The stretches of time in which at least one voice of a polyphonic phrase plays a note or a chord, in order and
   * without overlaps: the (start, end) of every element of every voice that is not a rest, joined where they overlap or
   * are at most [[TOLERANCE_NS]] apart.
   */
  def soundingIntervalsNS(phrase: Phrase): List[(BigInt, BigInt)] = {
    require(phrase.polyphony)
    val intervals = phrase.musicalElements
      .collect { case voice: Phrase => voice.musicalElements }
      .flatten
      .filterNot(_.isInstanceOf[Rest])
      .map(element => (element.getStartTimeNS, element.getEndTimeNS))
      .sortBy(_._1)

    intervals.foldLeft(List.empty[(BigInt, BigInt)]) {
      case ((from, until) :: joined, (start, end)) if start <= until + TOLERANCE_NS =>
        (from, until.max(end)) :: joined
      case (joined, interval) =>
        interval :: joined
    }.reverse
  }
}
