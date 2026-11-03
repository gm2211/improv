package instruments

import java.util.concurrent.Executors
import javax.sound.midi.Sequence

import designPatterns.observer.{EventNotification, Observer}
import instruments.InstrumentType.{CHROMATIC_PERCUSSION, InstrumentType, PERCUSSIVE, PIANO}
import midi.MIDIPlayer
import org.jfugue.midi.MidiParserListener
import org.jfugue.pattern.Pattern
import org.jfugue.theory
import org.slf4j.LoggerFactory
import org.staccato.StaccatoParser
import representation._
import utils.ImplicitConversions.{anyToRunnable, toDouble, toEnhancedIterable}

import scala.util.Try

class JFugueInstrument(override val instrumentType: InstrumentType = InstrumentType.randomInstrument) extends AsyncInstrument with Observer {
  private val log = LoggerFactory.getLogger(getClass)
  private val threadPool = Executors.newSingleThreadExecutor()
  val player = new MIDIPlayer
  player.addObserver(this)

  override def play(phrase: Phrase): Unit = {
    if (player.playing) {
      log.debug("Still playing")
      return
    }

    val musicSequence = JFugueUtils.toSequence(phrase, instrumentType)

    threadPool.submit(new Runnable {
      override def run(): Unit = player.play(musicSequence)
    })
  }

  override def notify(eventNotification: EventNotification): Unit = {
    eventNotification match {
      case MIDIPlayer.FinishedPlaying =>
        notifyObservers(AsyncInstrument.FinishedPlaying)
    }
  }
}


object JFugueUtils {
  val log = LoggerFactory.getLogger(getClass)
  val MAX_VOICE: Int = 15

  def createPattern(phrase: Phrase, instrumentType: InstrumentType): Pattern = {
    val tempo = Try(phrase.tempoBPM).getOrElse(MusicalElement.DEFAULT_TEMPO_BPM) * 4
    createPatternHelper(phrase, instrumentType.instrumentNumber - 1, tempo)
      .setInstrument(instrumentType.instrumentNumber - 1)
      .setTempo(tempo.toInt)
  }

  def createPatternHelper(musicalElement: MusicalElement, instrumentNumber: Int, tempoBPM: Double): Pattern = {
    val pattern: Pattern = musicalElement match {
      case note: Note =>
        convertNote(note, instrumentNumber, tempoBPM).getPattern
      case rest: Rest =>
        theory.Note.createRest(rest.getDurationBPM(tempoBPM)).getPattern
      case chord: Chord =>
        createChordPattern(chord, instrumentNumber, tempoBPM)
      case phrase: Phrase =>
        createPhrasePattern(phrase, instrumentNumber)
    }
    if (PERCUSSIVE.range.contains(instrumentNumber)) {
      pattern.setVoice(9)
    }
    pattern
  }

  def convertPolyphonicPhrase(phrase: Phrase, instrumentNumber: Int): String = {
    Option.when(phrase.polyphony) {
      phrase.musicalElements.asInstanceOf[List[Phrase]].zipped.map { case musicalElements =>
        musicalElements.map {
          case Some(elem) =>
            s"@${elem.getStartTimeBPM(phrase.tempoBPM).toDouble} ${createPatternHelper(elem, instrumentNumber, phrase.tempoBPM)}"
          case _ =>
            ""
        }.mkString(" ")
      }.mkString(" ")
    }.getOrElse("")
  }

  def createPhrasePattern(phrase: Phrase, instrumentNumber: Int): Pattern = phrase match {
    case polyphonicPhrase@Phrase(_, true, _, _) =>
      new Pattern(convertPolyphonicPhrase(polyphonicPhrase, instrumentNumber))
    case normalPhrase@Phrase(_, false, _, _) =>
      new Pattern(normalPhrase.map(createPatternHelper(_, instrumentNumber, phrase.tempoBPM).toString).mkString(" "))
  }

  def createChordPattern(chord: Chord, instrumentNumber: Int, tempoBPM: Double): Pattern =
    new Pattern(chord.notes.map(convertNote(_, instrumentNumber, tempoBPM).getPattern.toString).mkString("+"))

  def convertNote(note: Note, instrumentNumber: Int, tempoBPM: Double): theory.Note = {
    new theory.Note(s"${note.name.toString}${note.accidental.toString}${note.octave}")
      .setDuration(note.getDurationBPM(tempoBPM))
      .setOnVelocity(note.loudness.loudness.toByte)
      .setOffVelocity(0)
      .setPercussionNote(PERCUSSIVE.range.contains(instrumentNumber) ||
                         CHROMATIC_PERCUSSION.range.contains(instrumentNumber))
  }

  def mergePatterns(patterns: Iterable[Pattern]): String = {
    var phrasePatternString = ""
    for ((curPattern, voice) <- patterns.to(LazyList).zipWithIndex) {
      val nonPercussionVoice = if (voice < 9) voice + 1 else voice + 1
      if (nonPercussionVoice <= MAX_VOICE) {
        phrasePatternString += s" ${curPattern.setVoice(nonPercussionVoice).toString}"
      }
    }
    phrasePatternString
  }

  def toSequence(phrase: Phrase, instrumentType: InstrumentType): Sequence =
    toSequence(createPattern(phrase, instrumentType))

  def toSequence(pattern: Pattern): Sequence = {
    val parser = new StaccatoParser()
    val midiListener = new MidiParserListener()
    parser.addParserListener(midiListener)
    parser.parse(pattern)
    midiListener.getSequence
  }
}
