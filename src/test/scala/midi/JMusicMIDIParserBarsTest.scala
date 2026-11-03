package midi

import midi.MidiTestFiles.NoteTrack
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import representation.{Note, Phrase}

import java.io.File
import java.nio.file.{Files, Path}

/** The case library cuts a part into phrases on bar lines, from the time signatures of the file (#27). */
class JMusicMIDIParserBarsTest extends AnyFlatSpec with Matchers with BeforeAndAfterAll {
  private val dir: Path = Files.createTempDirectory("jmusic-parser-bars-test")

  override def afterAll(): Unit = {
    def delete(file: File): Unit = {
      Option(file.listFiles).foreach(_.foreach(delete))
      file.delete()
    }
    delete(dir.toFile)
  }

  /** Quarter notes, one per beat from beat 0, each with the pitch 36 + its beat, so a phrase says which beat it starts on. */
  private def beatPitches(beats: Int): Seq[Int] = Seq.tabulate(beats)(36 + _)

  private def phrasesOf(parser: JMusicMIDIParser): List[Phrase] = {
    val part = parser.getPartIndexByInstrument.values.flatten.toList.sorted.find(parser.getMultiVoicePhrases(_).nonEmpty).get
    parser.getMultiVoicePhrases(part)
  }

  private def firstBeats(phrases: List[Phrase]): List[Int] =
    phrases.map(_.collectFirst { case note: Note => note.midiPitch - 36 }.get)

  private def lengthsInBeats(phrases: List[Phrase], bpm: Double): List[Double] =
    phrases.map(_.getDurationBPM(bpm).toDouble)

  private def synthetic(name: String, beats: Int, bpm: Double, timeSignatures: (Int, Int, Int)*): JMusicMIDIParser = {
    val file = MidiTestFiles.write(
      dir.resolve(name),
      Seq(NoteTrack(channel = 0, pitches = beatPitches(beats))),
      tempos = Seq(0 -> bpm),
      timeSignatures = timeSignatures)
    JMusicMIDIParser(file.toString)
  }

  "The midi parser" should "cut a 4/4 file into phrases of 2 bars that start on the bar lines" in {
    // 50 beats at 120 BPM: 6 phrases of 8 beats (4 seconds), the 2 beats after the last bar line are dropped.
    // (A 5 second cut would give 10 beat phrases starting at beats 0, 10, 20, ...)
    val phrases = phrasesOf(synthetic("four-four.mid", beats = 50, bpm = 120, timeSignatures = (0, 4, 4)))

    firstBeats(phrases) shouldBe List(0, 8, 16, 24, 32, 40)
    lengthsInBeats(phrases, 120) shouldBe List.fill(6)(8.0)
    phrases.foreach(_.getDurationNS shouldBe BigInt(4000000000L))
  }

  it should "cut a 3/4 file into phrases of 6 beats" in {
    val phrases = phrasesOf(synthetic("three-four.mid", beats = 40, bpm = 75, timeSignatures = (0, 3, 4)))

    // 36 of the 40 beats make 6 whole phrases of 2 bars; 75 BPM: 6 beats are 4.8 seconds
    firstBeats(phrases) shouldBe List(0, 6, 12, 18, 24, 30)
    lengthsInBeats(phrases, 75) shouldBe List.fill(6)(6.0)
    phrases.foreach(_.getDurationNS shouldBe BigInt(4800000000L))
  }

  it should "keep the number of beats of a phrase when the tempo changes" in {
    // the same notes at 60 BPM take twice as long, and are cut in the same places
    val phrases = phrasesOf(synthetic("four-four-slow.mid", beats = 50, bpm = 60, timeSignatures = (0, 4, 4)))

    firstBeats(phrases) shouldBe List(0, 8, 16, 24, 32, 40)
    phrases.foreach(_.getDurationNS shouldBe BigInt(8000000000L))
  }

  it should "cut on the bar lines of each section when the time signature changes" in {
    // 4/4 for 3 bars (12 beats), then 3/4 up to beat 40. Bars start at 0, 4, 8 | 12, 15, 18, 21, 24, 27, 30, 33, 36
    // phrases of 2 bars: beats 0-8, 8-15 (a 4/4 and a 3/4 bar), 15-21, 21-27, 27-33, 33-39
    val phrases = phrasesOf(synthetic("change.mid", beats = 40, bpm = 120, timeSignatures = (0, 4, 4), (12, 3, 4)))

    firstBeats(phrases) shouldBe List(0, 8, 15, 21, 27, 33)
    lengthsInBeats(phrases, 120) shouldBe List(8.0, 7.0, 6.0, 6.0, 6.0, 6.0)
  }

  it should "assume 4/4 for a file with no time signature" in {
    val file = MidiTestFiles.write(
      dir.resolve("none.mid"),
      Seq(NoteTrack(channel = 0, pitches = beatPitches(50))),
      tempos = Seq(0 -> 120.0))
    MidiTimeSignature.read(file.toString) shouldBe empty

    val phrases = phrasesOf(JMusicMIDIParser(file.toString))

    firstBeats(phrases) shouldBe List(0, 8, 16, 24, 32, 40)
    lengthsInBeats(phrases, 120) shouldBe List.fill(6)(8.0)
  }

  it should "assume 4/4 and 120 BPM for a file with neither a time signature nor a tempo" in {
    val file = MidiTestFiles.write(dir.resolve("neither.mid"), Seq(NoteTrack(channel = 0, pitches = beatPitches(50))))

    val parser = JMusicMIDIParser(file.toString)

    firstBeats(phrasesOf(parser)) shouldBe List(0, 8, 16, 24, 32, 40)
  }

  it should "cut the phrases of the case library and the phrases of every part the same way" in {
    val file = MidiTestFiles.write(
      dir.resolve("two-parts.mid"),
      Seq(
        NoteTrack(channel = 0, pitches = beatPitches(50)),
        NoteTrack(channel = 1, pitches = beatPitches(50))),
      tempos = Seq(0 -> 120.0),
      timeSignatures = Seq((0, 4, 4)))
    val parser = JMusicMIDIParser(file.toString)

    val parts = parser.getPartIndexByInstrument.values.flatten.toList.sorted.filter(parser.getMultiVoicePhrases(_).nonEmpty)
    parts should have size 2
    parts.map(part => firstBeats(parser.getMultiVoicePhrases(part))).toSet shouldBe Set(List(0, 8, 16, 24, 32, 40))
  }

  // trainingMIDIs/*.mid: the length in beats of the first phrases of a part, from the time signature events of the file
  // (read with javax.sound.midi; every one of the 76 files has an event on tick 0, 7 of them change). A phrase is 2 bars:
  //  - constant signatures: 3/4 = 6 beats, 2/2 = 8, 2/4 = 4, 4/4 = 8
  //  - angels-among-us.mid: 4/4 for 64 beats (8 phrases), one bar of 2/4 (bar 17, so the 9th phrase is 2 + 4 beats), 4/4
  //  - the-city-put-the-country-in-me.mid, baby-likes-to-rock-it.mid: one or two bars of 5/4 first, then 4/4
  //    (10 beats = 2 bars of 5/4; 9 beats = the 5/4 bar and the first 4/4 bar)
  //  - jdenver-poemsprayers.mid: 26 bars of 4/4 (13 phrases), a 5/4 bar (9 beats with the next 4/4 bar), 4/4 for 8 bars
  //    (3 more phrases), then a 2/4 bar (6 beats with the 4/4 bar before it), 4/4 again
  private val phraseLengths = List(
    "cajun-fiddle.mid" -> List.fill(8)(8.0),
    "jdenver-anniessong.mid" -> List.fill(8)(6.0),
    "ive-got-spurs-that-jingle-jangle-jingle.mid" -> List.fill(8)(8.0),
    "wildwood-flower.mid" -> List.fill(8)(4.0),
    "angels-among-us.mid" -> (List.fill(8)(8.0) ++ List(6.0, 8.0, 8.0)),
    "the-city-put-the-country-in-me.mid" -> (10.0 :: List.fill(6)(8.0)),
    "baby-likes-to-rock-it.mid" -> (9.0 :: List.fill(6)(8.0)),
    "jdenver-poemsprayers.mid" -> (List.fill(13)(8.0) ++ List(9.0, 8.0, 8.0, 8.0, 6.0, 8.0)))

  phraseLengths.foreach { case (resource, expected) =>
    it should s"cut $resource into phrases of ${expected.distinct.mkString(" / ")} beats" in {
      val parser = JMusicMIDIParser(MidiTestFiles.copyResource(s"trainingMIDIs/$resource", dir.resolve(resource)).toString)
      val parts = parser.getPartIndexByInstrument.values.flatten.toList.map(parser.getMultiVoicePhrases)

      // every part is cut in the same places, so any of them tells; take the longest
      val phrases = parts.maxBy(_.size)
      lengthsInBeats(phrases.take(expected.size), parser.score.getTempo).map(beats => Math.round(beats * 1000) / 1000.0) shouldBe expected
    }
  }

  it should "read the time signatures of the file, not what jMusic makes of them" in {
    val file = MidiTestFiles.copyResource("trainingMIDIs/angels-among-us.mid", dir.resolve("angels-among-us-signatures.mid"))

    // 192 ticks to a quarter note: 64 beats of 4/4, one bar of 2/4, then 4/4 again
    MidiTimeSignature.read(file.toString).map(change => (change.startBeat, change.signature.toString)) shouldBe
      List((BigDecimal(0), "4/4"), (BigDecimal(64), "2/4"), (BigDecimal(66), "4/4"))
  }
}
