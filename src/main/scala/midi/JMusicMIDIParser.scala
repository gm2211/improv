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
    new JMusicMIDIParser(
      score,
      PhraseSegmenter.byBars(MidiTimeSignature.read(filename), score.getTempo),
      MidiKeySignature.read(filename))
  }

  def apply(phraseSegmenter: PhraseSegmenter)(filename: String) = {
    val score: Score = getScore(filename)
    new JMusicMIDIParser(score, phraseSegmenter, MidiKeySignature.read(filename))
  }

  private def getScore(filename: String): Score = {
    val score: Score = new Score()
    Read.midi(score, filename)
    // jMusic keeps one tempo per score, the last tempo event it read (or a garbled one, see MidiTempo): use the tempo
    // the file spends the longest time in, which is what the phrase length in seconds is measured against (#23)
    MidiTempo.longestLastingBPM(filename).foreach(score.setTempo)
    // and one key, the last key signature event: use the key the file spends the longest time in, the key of a whole
    // part (a phrase cut out of it gets the key of its own stretch of the piece, see JMusicMIDIParser, #41)
    MidiKeySignature.longestLasting(filename).foreach { key =>
      score.setKeySignature(if (key.sharps > 0) key.sharps else -key.flats)
      score.setKeyQuality(if (key.quality == Minor) 1 else 0)
    }
    score
  }

  /** MIDI channel 10 (index 9) is the General MIDI drum channel: its notes are drum sounds, not pitches. */
  val DRUM_CHANNEL = 9

  def isDrumPart(part: jmData.Part): Boolean = part.getChannel == DRUM_CHANNEL
}

/**
 * @param keySignatures the key signature events of the file (see [[MidiKeySignature]]). With events, every phrase cut out
 *                      of a part is in the key that lasts longest in its own stretch of the piece, so the phrases before
 *                      a modulation keep the key they are in (#41); without, every phrase has the key of the score.
 */
class JMusicMIDIParser(
  val score: jmData.Score,
  val phraseSegmenter: PhraseSegmenter,
  val keySignatures: Seq[KeySignatureChange] = Nil) extends MIDIParser {

  def split(phrase: Phrase): Iterable[Phrase] = {
    val pieces = phraseSegmenter.segmentWithOffsets(phrase)
    if (keySignatures.isEmpty) pieces.map(_._2)
    else pieces.map { case (offsetNS, piece) => piece.withKeySignature(keyOf(offsetNS, offsetNS + piece.getDurationNS)) }
  }

  /**
   * The key that lasts longest from `fromNS` to `untilNS`. The events are in beats and become time with the tempo of
   * the score, the one tempo that every note time of the piece and every bar line were converted with.
   */
  private def keyOf(fromNS: BigInt, untilNS: BigInt): KeySignature = {
    val tempoBPM = BigDecimal(score.getTempo)
    KeySignatureChange.longestIn(
      keySignatures,
      MusicalElement.toBPM(fromNS, tempoBPM),
      MusicalElement.toBPM(untilNS, tempoBPM))
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
   * event it read, which `JMusicMIDIParser(filename)` replaces with the key the file spends the longest time in (#41);
   * C major when there is none. A phrase cut out of a part gets the key of its own stretch of the piece instead (see
   * [[JMusicMIDIParser]]).
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

    // addToPhraseElements fills a gap of more than Phrase.GAP_TOLERANCE_NS with a rest and ignores a shorter one: the
    // nanoseconds jMusic's double times leave between two notes that touch are not rests (#39)
    for (jmNote <- jmNotes) {
      convertNote(jmNote).foreach(Phrase.addToPhraseElements(_, elements))
    }

    new Phrase(elements.toList, tempoBPM = getTempo(phrase), keySignature = getKeySignature(phrase))
  })
}

