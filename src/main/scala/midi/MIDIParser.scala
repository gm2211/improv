package midi

import instruments.InstrumentType.InstrumentType
import representation.Phrase

import scala.collection.mutable

trait MIDIParserFactory {
  def apply(filename: String): MIDIParser
}

trait MIDIParser {

  def getPhrases(partNum: Int): Iterable[Phrase]

  def getMultiVoicePhrases(partNum: Int): List[Phrase]

  def getAllMultiVoicePhrases: List[List[Phrase]]

  def getPartIndexByInstrument: mutable.MultiMap[InstrumentType, Int]
}
