package midi

import midi.MidiTestFiles.NoteTrack
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import representation.KeySignature.{Major, Minor}
import representation.{KeySignature, Phrase}

import java.io.File
import java.nio.file.{Files, Path}
import javax.sound.midi.{MetaMessage, MidiSystem}

/**
 * Every phrase the parser yields is in the key of its file: the phrases of the case library, their voices, the merged
 * phrases and the whole part (#36). Before, a phrase got the key only as the wrapper of a whole polyphonic part, and
 * cutting the part lost it; and the key was always a minor one, because the quality was read from the part and jMusic
 * only sets it on the score.
 */
class JMusicMIDIParserKeyTest extends AnyFlatSpec with Matchers with BeforeAndAfterAll {
  private val dir: Path = Files.createTempDirectory("jmusic-parser-key-test")

  override def afterAll(): Unit = {
    def delete(file: File): Unit = {
      Option(file.listFiles).foreach(_.foreach(delete))
      file.delete()
    }
    delete(dir.toFile)
  }

  private def parserOf(file: Path): JMusicMIDIParser = JMusicMIDIParser(file.toString)

  private def parserOfResource(resource: String): JMusicMIDIParser =
    parserOf(MidiTestFiles.copyResource(s"trainingMIDIs/$resource", dir.resolve(resource)))

  private def voicesOf(phrase: Phrase): List[Phrase] =
    if (phrase.polyphony) phrase.musicalElements.asInstanceOf[List[Phrase]] else List(phrase)

  private def pitchedParts(parser: JMusicMIDIParser): List[Int] = parser.getPartIndexByInstrument.values.flatten.toList.sorted

  /** 64 quarter notes (8 phrases of 2 bars in 4/4), in a track with a second voice or not, with the given key. */
  private def fileInKey(name: String, key: Option[(Int, Boolean)], polyphonic: Boolean): Path = {
    val pitches = Seq.tabulate(64)(60 + _ % 12)
    val track = NoteTrack(channel = 0, pitches = pitches, harmony = if (polyphonic) pitches.map(_ - 12) else Nil)
    MidiTestFiles.write(dir.resolve(name), Seq(track), keySignatures = key.map { case (sharps, minor) => (0, sharps, minor) }.toList)
  }

  /** Everything the parser yields for the file, each with the key it carries. */
  private def keysOf(parser: JMusicMIDIParser): Map[String, Set[KeySignature]] = {
    val parts = pitchedParts(parser)
    Map(
      "whole part" -> parts.flatMap(parser.getMultiVoicePhrase).map(_.keySignature).toSet,
      "library phrases" -> parts.flatMap(parser.getMultiVoicePhrases).map(_.keySignature).toSet,
      "voices" -> parts.flatMap(parser.getMultiVoicePhrases).flatMap(voicesOf).map(_.keySignature).toSet,
      "merged phrases" -> parts.flatMap(parser.getPhrases).map(_.keySignature).toSet)
  }

  private def checkAllIn(parser: JMusicMIDIParser, key: KeySignature): Unit =
    keysOf(parser).foreach { case (what, keys) =>
      withClue(s"$what: ") { keys shouldBe Set(key) }
    }

  "The phrases of a polyphonic part" should "all be in the key of the file" in {
    val parser = parserOf(fileInKey("two-flats.mid", Some((-2, false)), polyphonic = true))

    val phrases = parser.getMultiVoicePhrases(pitchedParts(parser).head)
    phrases should have size 8
    phrases.foreach(_.polyphony shouldBe true) // the key used to be lost when the part was cut, and on its voices
    checkAllIn(parser, KeySignature(flats = 2, sharps = 0, Major))
  }

  "The phrases of a part with a single voice" should "all be in the key of the file" in {
    val parser = parserOf(fileInKey("three-sharps.mid", Some((3, false)), polyphonic = false))

    parser.getMultiVoicePhrases(pitchedParts(parser).head).foreach(_.polyphony shouldBe false)
    checkAllIn(parser, KeySignature(flats = 0, sharps = 3, Major))
  }

  "A minor key" should "be read as minor, with the signature of its relative major" in {
    checkAllIn(
      parserOf(fileInKey("c-minor.mid", Some((-3, true)), polyphonic = true)),
      KeySignature(flats = 3, sharps = 0, Minor))
    checkAllIn(
      parserOf(fileInKey("c-sharp-minor.mid", Some((4, true)), polyphonic = false)),
      KeySignature(flats = 0, sharps = 4, Minor))
  }

  "A file without a key signature event" should "be in C major, not in A minor" in {
    checkAllIn(parserOf(fileInKey("no-key-poly.mid", None, polyphonic = true)), Phrase.DEFAULT_KEY_SIGNATURE)
    checkAllIn(parserOf(fileInKey("no-key-mono.mid", None, polyphonic = false)), Phrase.DEFAULT_KEY_SIGNATURE)
    Phrase.DEFAULT_KEY_SIGNATURE shouldBe KeySignature(flats = 0, sharps = 0, Major)
  }

  // The key signature events of a file (type 0x59), read with javax.sound.midi: (sharps, minor)
  private def keyEventsOf(file: Path): List[(Int, Boolean)] =
    MidiSystem.getSequence(file.toFile).getTracks.toList.flatMap { track =>
      (0 until track.size).flatMap { i =>
        track.get(i).getMessage match {
          case meta: MetaMessage if meta.getType == 0x59 => Some((meta.getData()(0).toInt, meta.getData()(1) == 1))
          case _ => None
        }
      }
    }

  // 3 sharps (all-my-exes-live-in-texas.mid), 4 sharps (back-home-again.mid), none (cajun-fiddle.mid: C major)
  List("all-my-exes-live-in-texas.mid", "back-home-again.mid", "cajun-fiddle.mid").foreach { resource =>
    "Every phrase of " + resource should "be in the key that the file declares" in {
      val parser = parserOfResource(resource)
      val events = keyEventsOf(dir.resolve(resource))
      events should have size 1 // one key for the whole file: what the parser can tell
      val (sharps, minor) = events.head
      val expected = KeySignature(
        flats = scala.math.max(-sharps, 0),
        sharps = scala.math.max(sharps, 0),
        if (minor) Minor else Major)

      pitchedParts(parser) should not be empty
      parser.getMultiVoicePhrases(pitchedParts(parser).head) should not be empty
      checkAllIn(parser, expected)
    }
  }

  // Key changes inside a file (#41): a phrase is in the key that lasts longest in it, not in the last key of the file

  /** 64 quarter notes (8 phrases of 2 bars in 4/4: beats 0-8, 8-16, ...), with the given key events (beat, sharps). */
  private def fileWithKeys(name: String, keys: Seq[(Int, Int)], polyphonic: Boolean): Path = {
    val pitches = Seq.tabulate(64)(60 + _ % 12)
    val track = NoteTrack(channel = 0, pitches = pitches, harmony = if (polyphonic) pitches.map(_ - 12) else Nil)
    MidiTestFiles.write(dir.resolve(name), Seq(track), keySignatures = keys.map { case (beat, sharps) => (beat, sharps, false) })
  }

  private val bFlat = KeySignature(flats = 2, sharps = 0, Major)
  private val aMajor = KeySignature(flats = 0, sharps = 3, Major)

  /** The keys of the library phrases of the first part, of their voices and of the merged phrases, phrase by phrase. */
  private def phraseKeys(parser: JMusicMIDIParser): (List[KeySignature], List[Set[KeySignature]], List[KeySignature]) = {
    val part = pitchedParts(parser).head
    val phrases = parser.getMultiVoicePhrases(part)
    (phrases.map(_.keySignature), phrases.map(voicesOf(_).map(_.keySignature).toSet), parser.getPhrases(part).toList.map(_.keySignature))
  }

  "A file that changes key on a phrase boundary" should "have the phrases before the change in the first key" in {
    for (polyphonic <- List(true, false)) {
      // B flat for beats 0-24 (3 phrases), A from beat 24 (5 phrases): A lasts longer, and is also the last event
      val parser = parserOf(fileWithKeys(s"change-24-$polyphonic.mid", Seq(0 -> -2, 24 -> 3), polyphonic))
      val (keys, voiceKeys, mergedKeys) = phraseKeys(parser)

      withClue(s"polyphonic = $polyphonic: ") {
        keys shouldBe List.fill(3)(bFlat) ++ List.fill(5)(aMajor)
        voiceKeys shouldBe keys.map(Set(_))
        mergedKeys shouldBe keys
        parser.getMultiVoicePhrase(pitchedParts(parser).head).get.keySignature shouldBe aMajor
      }
    }
  }

  it should "give the whole part the key that lasts longest, not the last one" in {
    // B flat for 40 beats (5 phrases), A for the last 24 (3): jMusic keeps A, the last event
    val parser = parserOf(fileWithKeys("change-40.mid", Seq(0 -> -2, 40 -> 3), polyphonic = true))
    val whole = parser.getMultiVoicePhrase(pitchedParts(parser).head).get

    whole.keySignature shouldBe bFlat
    voicesOf(whole).map(_.keySignature).toSet shouldBe Set(bFlat)
    phraseKeys(parser)._1 shouldBe List.fill(5)(bFlat) ++ List.fill(3)(aMajor)
  }

  "A phrase that a key change falls inside" should "be in the key that lasts longest in it, the first key on a tie" in {
    // phrase 3 is beats 24-32: a change on beat 26 leaves 2 beats of B flat and 6 of A, on beat 30 6 of B flat and 2 of
    // A, on beat 28 (its middle bar line, where the 6 modulations of the training files fall) 4 of each
    def keysWithChangeAt(beat: Int) = phraseKeys(parserOf(fileWithKeys(s"change-$beat.mid", Seq(0 -> -2, beat -> 3), polyphonic = true)))._1
    keysWithChangeAt(26) shouldBe List.fill(3)(bFlat) ++ List.fill(5)(aMajor)
    keysWithChangeAt(30) shouldBe List.fill(4)(bFlat) ++ List.fill(4)(aMajor)
    keysWithChangeAt(28) shouldBe List.fill(4)(bFlat) ++ List.fill(4)(aMajor)
  }

  "A file that starts without a key event" should "be in C major until its first one" in {
    val (keys, _, _) = phraseKeys(parserOf(fileWithKeys("late-key.mid", Seq(16 -> 3), polyphonic = false)))
    keys shouldBe List.fill(2)(Phrase.DEFAULT_KEY_SIGNATURE) ++ List.fill(6)(aMajor)
  }

  // king-of-the-road.mid, read with javax.sound.midi: 4/4, 3 flats from tick 0, 4 sharps from beat 68, 1 flat from beat
  // 132 (ticks 32640 and 63360 at 480 a quarter). Phrases are 8 beats, so phrase 8 (beats 64-72) and phrase 16 (128-136)
  // have each key for 4 beats and keep the first; jMusic labelled every phrase 1 flat, the last event
  "Every phrase of king-of-the-road.mid" should "be in the key of its stretch of the song" in {
    val parser = parserOfResource("king-of-the-road.mid")
    val eFlat = KeySignature(flats = 3, sharps = 0, Major)
    val eMajor = KeySignature(flats = 0, sharps = 4, Major)
    val fMajor = KeySignature(flats = 1, sharps = 0, Major)
    def expected(phraseIndex: Int) = if (phraseIndex <= 8) eFlat else if (phraseIndex <= 16) eMajor else fMajor

    keyEventsOf(dir.resolve("king-of-the-road.mid")) shouldBe List((-3, false), (4, false), (-1, false))
    // most parts last the whole song, so they have phrases in all three keys
    pitchedParts(parser).count(parser.getMultiVoicePhrases(_).size > 17) should be > 0
    pitchedParts(parser).foreach { part =>
      val phrases = parser.getMultiVoicePhrases(part)
      withClue(s"part $part: ") {
        phrases.map(_.keySignature) shouldBe phrases.indices.map(expected).toList
        phrases.foreach(p => voicesOf(p).map(_.keySignature).toSet shouldBe Set(p.keySignature))
        parser.getPhrases(part).toList.map(_.keySignature) shouldBe phrases.indices.map(expected).toList
      }
    }
    // the whole song: 68 beats of E flat, 64 of E, and the rest (more than 68) of F
    parser.getMultiVoicePhrase(pitchedParts(parser).head).get.keySignature shouldBe fMajor
  }

  "Parsing a file" should "give the key of the file to a phrase that is built from a jMusic phrase on its own" in {
    val parser = parserOf(fileInKey("two-flats-direct.mid", Some((-2, false)), polyphonic = false))
    val jmPhrase = parser.score.getPart(pitchedParts(parser).head).getPhrase(0)

    JMusicParserUtils.convertPhrase(jmPhrase).keySignature shouldBe KeySignature(flats = 2, sharps = 0, Major)
    // a jMusic phrase that is in no part of a score has no key to read
    JMusicParserUtils.convertPhrase(new jm.music.data.Phrase()).keySignature shouldBe Phrase.DEFAULT_KEY_SIGNATURE
  }
}
