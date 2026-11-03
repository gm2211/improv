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

  def getFilters = (phrase: Phrase) => {
    !Phrase.allRest(phrase) &&
    !phrase.head.isInstanceOf[Rest] &&
    !phrase.last.isInstanceOf[Rest] &&
    !(phrase.collect{case r:Rest => r.getDurationNS}.sum >= (2.0/3.0 * phrase.getDurationNS.toLong).toLong)
  }
}


