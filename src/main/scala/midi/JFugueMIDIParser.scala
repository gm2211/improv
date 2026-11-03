package midi

import utils.collections.MultiMap
import java.io.File
import javax.sound.midi.MidiSystem

import instruments.InstrumentType.InstrumentType
import org.jfugue.midi.MidiParser
import org.jfugue.parser.ParserListener
import org.jfugue.theory.{Chord, Note}
import representation.Phrase
import utils.collections.CollectionUtils

import scala.collection.mutable

object JFugueMIDIParser extends MIDIParserFactory {
 override def apply(filename: String): JFugueMIDIParser = {
    val parser = new MidiParser()
    val parserListener = new JFugueParseListener
    parser.addParserListener(parserListener)
    parser.parse(MidiSystem.getSequence(new File(filename)))
    parserListener.buildStructure
  }

}

class JFugueMIDIParser extends MIDIParser {
  override def getPartIndexByInstrument: MultiMap[InstrumentType, Int] = CollectionUtils.createHashMultimap

  override def getPhrases(partNum: Int): Iterable[Phrase] = List()

  override def getMultiVoicePhrases(partNum: Int): List[Phrase] = List()

  override def getAllMultiVoicePhrases: List[List[Phrase]] = List()
}

class JFugueParseListener extends ParserListener {
  def buildStructure: JFugueMIDIParser = new JFugueMIDIParser //TODO pass parsed content into class

  override def afterParsingFinished(): Unit = ()

  override def onTrackBeatTimeBookmarkRequested(timeBookmarkId: String): Unit = ()

  override def onChordParsed(chord: Chord): Unit = println(s"Found chord: ${chord.toDebugString}")

  override def beforeParsingStarts(): Unit = ()

  override def onLyricParsed(lyric: String): Unit = ()

  override def onTrackBeatTimeRequested(time: Double): Unit = ()

  override def onControllerEventParsed(controller: Byte, value: Byte): Unit = ()

  override def onInstrumentParsed(instrument: Byte): Unit = ()

  override def onTimeSignatureParsed(numerator: Byte, powerOfTwo: Byte): Unit = ()

  override def onLayerChanged(layer: Byte): Unit = ()

  override def onTrackChanged(track: Byte): Unit = ()

  override def onKeySignatureParsed(key: Byte, scale: Byte): Unit = ()

  override def onBarLineParsed(id: Long): Unit = ()

  override def onMarkerParsed(marker: String): Unit = ()

  override def onNoteParsed(note: Note): Unit = ()

  override def onTempoChanged(tempoBPM: Int): Unit = ()

  override def onTrackBeatTimeBookmarked(timeBookmarkId: String): Unit = ()

  override def onSystemExclusiveParsed(bytes: Byte*): Unit = ()

  override def onChannelPressureParsed(pressure: Byte): Unit = ()

  override def onPolyphonicPressureParsed(key: Byte, pressure: Byte): Unit = ()

  override def onFunctionParsed(id: String, message: scala.Any): Unit = ()

  override def onPitchWheelParsed(lsb: Byte, msb: Byte): Unit = ()
}
