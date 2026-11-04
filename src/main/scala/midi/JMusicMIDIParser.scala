package midi

import utils.collections.MultiMap
import instruments.InstrumentType
import instruments.InstrumentType.InstrumentType
import jm.music.data.Score
import jm.music.{data => jmData}
import jm.util.Read
import org.slf4j.LoggerFactory
import representation.KeySignature.{KeyQuality, Major, Minor}
import representation._
import training.segmentation.PhraseSegmenter
import utils.ImplicitConversions.{toEnhancedIterable, toFasterMutableList}
import utils.NumericUtils
import utils.collections.CollectionUtils
import utils.functional.FunctionalUtils

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.concurrent.duration.{NANOSECONDS, TimeUnit}
import scala.util.Try

object JMusicMIDIParser extends MIDIParserFactory with Function[String, JMusicMIDIParser] {
  /**
   * Parses `filename` and cuts every part into phrases of [[PhraseSegmenter.DEFAULT_BARS_PER_PHRASE]] bars, starting at
   * bar 1 (#27). The bars come from the time signature events of the file and the tempo of the score; both are read
   * with `javax.sound.midi` ([[MidiTimeSignature]], [[MidiTempo]]), not through jMusic (#31).
   */
  override def apply(filename: String) = {
    val score: Score = getScore(filename)
    new JMusicMIDIParser(score, PhraseSegmenter.byBars(MidiTimeSignature.read(filename), score.getTempo))
  }

  def apply(phraseSegmenter: PhraseSegmenter)(filename: String) = {
    val score: Score = getScore(filename)
    new JMusicMIDIParser(score, phraseSegmenter)
  }

  private def getScore(filename: String): Score = {
    val score: Score = new Score()
    Read.midi(score, filename)
    // jMusic keeps one tempo per score, the last tempo event it read (or a garbled one, see MidiTempo): use the tempo
    // the file spends the longest time in, which is what the phrase length in seconds is measured against (#23)
    MidiTempo.longestLastingBPM(filename).foreach(score.setTempo)
    score
  }

  /** MIDI channel 10 (index 9) is the General MIDI drum channel: its notes are drum sounds, not pitches. */
  val DRUM_CHANNEL = 9

  def isDrumPart(part: jmData.Part): Boolean = part.getChannel == DRUM_CHANNEL
}

class JMusicMIDIParser(
  val score: jmData.Score,
  val phraseSegmenter: PhraseSegmenter) extends MIDIParser {

  def split(phrase: Phrase): Iterable[Phrase] = {
    phraseSegmenter.segment(phrase)
  }

  override def getAllMultiVoicePhrases: List[List[Phrase]] = {
    (0 until score.getPartList.size()).map(getMultiVoicePhrases).toList
  }

  def getMultiVoicePhrase(partNum: Int): Option[Phrase] =
    JMusicParserUtils.convertPart(score.getPart(partNum))

  private val getPhrasesM = FunctionalUtils.memoized[Int, Iterable[Phrase]]((partNum: Int) => {
    getMultiVoicePhrase(partNum).map(phrase => split(Phrase.mergePhrases(phrase))).getOrElse(List())
  })

  override def getPhrases(partNum: Int): Iterable[Phrase] = getPhrasesM(partNum)

  private val getMultiVoicePhrasesM = FunctionalUtils.memoized((partNum: Int) => {
    getMultiVoicePhrase(partNum).map(split).getOrElse(List())
  })

  override def getMultiVoicePhrases(partNum: Int): List[Phrase] = getMultiVoicePhrasesM(partNum).toList

  /**
   * Indices of the parts that are classified by instrument: every part except the drum parts (MIDI channel 10).
   * jMusic gives a drum part the program of its track (0 without a program change), which `InstrumentType.classify`
   * would take for a piano (or strings, bass, ...), so drum phrases would enter the case library as pitched cases
   * (#22). improv's musicians are pitched instruments; a drum part can still be read by its index (`getMultiVoicePhrases`).
   */
  private def pitchedPartIndices: Seq[Int] = {
    val partsArray = score.getPartArray
    (0 until partsArray.size).filterNot(index => JMusicMIDIParser.isDrumPart(partsArray(index)))
  }

  private val partIndexByInstrumentM = FunctionalUtils.memoized[MultiMap[InstrumentType, Int]]({
    val partsArray = score.getPartArray
    pitchedPartIndices
      .groupByMultiMap(index => InstrumentType.classify(partsArray(index).getInstrument))
  })

  override def getPartIndexByInstrument: MultiMap[InstrumentType, Int] = partIndexByInstrumentM

  def getInstrumentsCounts: Map[InstrumentType.InstrumentCategory, Int] = {
    val partsArray = score.getPartArray
    val instruments = pitchedPartIndices.map(index => InstrumentType.classify(partsArray(index).getInstrument))
    instruments.groupBy(identity).view.mapValues(_.length).toMap
  }
}

object JMusicParserUtils {
  private val log = LoggerFactory.getLogger(getClass)

  def getTempo(part: jmData.Part): Double = {
    Try(part.getTempo)
      .filter(_ >= 0)
      .orElse(Try(part.getMyScore.getTempo))
      .getOrElse(MusicalElement.DEFAULT_TEMPO_BPM)
  }

  def getTempo(phrase: jmData.Phrase): Double = {
    Try(phrase.getTempo)
      .filter(_ >= 0)
      .getOrElse(getTempo(phrase.getMyPart))
  }


  def convertNote(jmNote: jmData.Note): Option[MusicalElement] = {
    val tempoBPM = getTempo(jmNote.getMyPhrase)
    val fromBPM = (value: Double) => MusicalElement.fromBPM(value, tempoBPM)
    val duration = fromBPM(jmNote.getRhythmValue)
    val startTime = fromBPM(jmNote.getNoteStartTime.get)

    if (jmNote.isRest) {
      Some(
        Rest()
          .withDuration(duration)
          .withStartTime(startTime)
      )
    } else {
      val notePitch = jmNote.getPitchType match {
        case jmData.Note.MIDI_PITCH =>
          jmNote.getPitch
        case _ =>
          jmData.Note.freqToMidiPitch(jmNote.getFrequency)
      }
      val intonation = if (jmNote.isFlat) Flat else if (jmNote.isSharp) Sharp else Natural
      val (noteName, _, _) = Note.parseString(jmNote.getNote)
      val loudness = Loudness(jmNote.getDynamic)

      noteName.flatMap(name => Some(Note(name = name,
        octave = Note.pitchToOctave(notePitch),
        midiPitch = notePitch,
        durationNS = duration,
        accidental = intonation,
        loudness = loudness,
        startTimeNS = startTime)))
    }
  }

  /**
   * The key signature of the score the part belongs to: jMusic's MIDI reader keeps one per score, the last key signature
   * event it read (a file with a key change gets its last key for the whole piece), and C major when there is none.
   * Both the signature and the quality are read from the score: the reader never sets the quality of a part, which stays
   * at jMusic's "undefined" value (`Integer.MIN_VALUE`), so reading it there made every key a minor one.
   */
  def extractSignature(part: jmData.Part): KeySignature = {
    val score = part.getMyScore
    val midiSignature = score.getKeySignature
    var numOfFlats = 0
    var numOfSharps = 0

    if (midiSignature > 0) {
      numOfSharps = midiSignature
    } else {
      numOfFlats = midiSignature * -1
    }

    val keyQuality = if (score.getKeyQuality == 0) Major else Minor

    KeySignature(flats = numOfFlats, sharps = numOfSharps, keyQuality)
  }

  /** The key signature of the score of the phrase's part; the default one for a phrase that is not in a part of a score. */
  def getKeySignature(phrase: jmData.Phrase): KeySignature =
    Option(phrase.getMyPart)
      .filter(_.getMyScore != null)
      .map(extractSignature)
      .getOrElse(Phrase.DEFAULT_KEY_SIGNATURE)

  val convertPart = FunctionalUtils.memoized((part: jmData.Part) => {
    val phrases = part.getPhraseList.asScala.map(convertPhrase)
    val keySignature = extractSignature(part)
    Phrase.createPolyphonicPhrase(phrases.toList, getTempo(part), keySignature)
  })

  val convertPhrase = FunctionalUtils.memoized((phrase: jmData.Phrase) => {
    val elements = ListBuffer[MusicalElement]()
    val jmNotes = phrase.getNoteList.asScala.toList.sortBy(_.getNoteStartTime.get)
    val errorMargin = BigInt(1000)
    var prevEnd = BigInt(0)

    for (jmNote <- jmNotes) {
      val convertedNote = convertNote(jmNote)
      val gap = convertedNote.map(_.getStartTimeNS - prevEnd).getOrElse(BigInt(0))
      if ( gap > errorMargin) {
        Phrase.addToPhraseElements(Rest(durationNS = gap), elements)
      }
      convertedNote.foreach(Phrase.addToPhraseElements(_, elements))
      convertedNote.foreach(n => prevEnd = NumericUtils.max(n.getEndTimeNS, prevEnd))
    }

    new Phrase(elements.toList, tempoBPM = getTempo(phrase), keySignature = getKeySignature(phrase))
  })
}

