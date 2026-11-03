package actors.composers

import cbr.MusicalCase
import instruments.InstrumentType.InstrumentType
import midi.{JMusicMIDIParser, MIDIParser, MIDIParserFactory}
import representation.Phrase
import utils.builders.{AtLeastOnce, Count, IsAtLeastOnce, Zero}

case class MIDIReaderComposerBuilder[
  FileNameCount <: Count,
  PartNumCount <: Count](
    filename: Option[String] = None,
    partNum: Option[Int] = None,
    midiParserFactory: Option[MIDIParserFactory] = None){

  def withFilename(filename: String) = copy[AtLeastOnce, PartNumCount](filename = Some(filename))

  def withPartNum(partNum: Int) = copy[FileNameCount, AtLeastOnce](partNum = Some(partNum))

  def withMIDIParser(midiParserFactory: MIDIParserFactory) = copy[FileNameCount, PartNumCount](midiParserFactory = Some(midiParserFactory))

  def build[A <: FileNameCount : IsAtLeastOnce, B <: PartNumCount : IsAtLeastOnce] =
    new MIDIReaderComposer(this.asInstanceOf[MIDIReaderComposerBuilder[AtLeastOnce, AtLeastOnce]])
}

object MIDIReaderComposer {
  def builder = new MIDIReaderComposerBuilder[Zero, Zero]
}

class MIDIReaderComposer(builder: MIDIReaderComposerBuilder[AtLeastOnce, AtLeastOnce]) extends Composer {
  val filename: String = builder.filename.get
  val partNum: Int = builder.partNum.get
  val midiReader: MIDIParser = builder.midiParserFactory.getOrElse(JMusicMIDIParser)(filename)
  val phraseIterator: Iterator[Phrase] = midiReader.getMultiVoicePhrases(partNum).toIterator

  override def compose(
      previousPhrase: Option[MusicalCase] = None,
      phrasesByOthers: Iterable[MusicalCase],
      constraints: List[MusicalCase => Boolean]): Option[Phrase] = {
    if (phraseIterator.hasNext)
      Some(phraseIterator.next())
    else
      None
  }
}
