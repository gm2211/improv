package midi

import javax.sound.midi.Sequence

import jm.midi.MidiUtil
import jm.music.data.{Part, Score}
import jm.music.{data => jmData}
import representation.{MusicalElement, Note, Phrase, Rest}
import utils.ImplicitConversions.toDouble

object MIDIConverter {
  def toSequence(phrase: Phrase): Sequence = {
    toSequence(JMusicConverterUtils.toPart(phrase))
  }

  def toSequence(part: jmData.Part): Sequence = {
    MidiUtil.scoreToSeq(JMusicConverterUtils.wrapInScore(part))
  }

}

object JMusicConverterUtils {

  def convertNote(note: Note, tempoBPM: Int): jmData.Note = {
    val jmNote = new jmData.Note()
    val duration: BigDecimal = MusicalElement.toBPM(note.durationNS, tempoBPM)
    jmNote.setDuration(duration)
    jmNote.setRhythmValue(duration / jmData.Note.DEFAULT_DURATION_MULTIPLIER)
    jmNote.setPitchType(jmData.Note.MIDI_PITCH)
    jmNote.setPitch(note.midiPitch)
    jmNote.setDynamic(note.loudness.loudness)
    jmNote
  }

  def convertRest(rest: Rest): jmData.Note =
    new jmData.Rest(rest.getDurationNS.toDouble)

  def toNote(musicalElement: MusicalElement, tempoBPM: Int): Option[jmData.Note] = musicalElement match {
    case note: Note =>
      Some(convertNote(note, tempoBPM))
    case rest: Rest =>
      Some(convertRest(rest))
    case _ =>
      None
  }


  def toPart(phrase: Phrase): jmData.Part = {
    val part = new jmData.Part()
    var phrases = List(phrase)

    if (phrase.polyphony) {
       phrases = phrase.musicalElements.asInstanceOf[List[Phrase]]
    }

    phrases.map(toPhrase).foreach{ phrase => phrase.setMyPart(part); part.add(phrase) }
    part.setTempo(phrase.tempoBPM)
    part
  }

  def toPhrase(phrase: Phrase): jmData.Phrase = {
    require(! phrase.polyphony)
    val jmPhrase = new jmData.Phrase()
    val startTimeOrdering = Ordering.by((elem: MusicalElement) => elem.getStartTimeNS)
    jmPhrase.setDuration(phrase.getDurationBPM(phrase.tempoBPM))
    jmPhrase.setStartTime(phrase.getStartTimeBPM(phrase.tempoBPM))
    jmPhrase.setTempo(phrase.tempoBPM)

    phrase.musicalElements
      .sorted(using startTimeOrdering)
      .flatMap(toNote(_, phrase.tempoBPM.toInt))
      .foreach{ note => note.setMyPhrase(jmPhrase); jmPhrase.add(note) }

    jmPhrase
  }

  def wrapInScore(jmPhrase: jmData.Phrase): Score = {
    val part = new Part()
    jmPhrase.setMyPart(part)
    part.addPhrase(jmPhrase)
    part.setTempo(jmPhrase.getTempo)
    val score = new Score()
    part.setMyScore(score)
    score.setTempo(part.getTempo)
    score.addPart(part)
    score
  }

  def wrapInScore(jmPart: jmData.Part): Score = {
    val score = new Score()
    jmPart.setMyScore(score)
    score.setTempo(jmPart.getTempo)
    score.addPart(jmPart)
    score
  }
}
