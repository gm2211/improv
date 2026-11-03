package midi

import midi.MidiTestFiles.NoteTrack
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.file.{Files, Path}

/** The tempo of a score is the one its MIDI file spends the longest time in, not the one jMusic ends up with (#23). */
class JMusicMIDIParserTempoTest extends AnyFlatSpec with Matchers with BeforeAndAfterAll {
  private val dir: Path = Files.createTempDirectory("jmusic-parser-tempo-test")

  override def afterAll(): Unit = {
    def delete(file: File): Unit = {
      Option(file.listFiles).foreach(_.foreach(delete))
      file.delete()
    }
    delete(dir.toFile)
  }

  private def parserOf(resource: String): JMusicMIDIParser =
    JMusicMIDIParser(MidiTestFiles.copyResource(s"trainingMIDIs/$resource", dir.resolve(resource)).toString)

  "The midi parser" should "use the tempo that lasts longest, not the last one, for a file that ends with a ritardando" in {
    // 64 quarter notes: 120 BPM for the first 60 beats, then 60 BPM (jMusic keeps the last event: 60)
    val file = MidiTestFiles.write(
      dir.resolve("ritardando.mid"),
      Seq(NoteTrack(channel = 0, pitches = Seq.tabulate(64)(60 + _ % 12))),
      tempos = Seq(0 -> 120.0, 60 -> 60.0))
    val parser = JMusicMIDIParser(file.toString)
    val partIndex = parser.getPartIndexByInstrument.values.flatten.head

    parser.score.getTempo shouldBe 120.0 +- 1e-4
    JMusicParserUtils.getTempo(parser.score.getPart(partIndex)) shouldBe 120.0 +- 1e-4
    val phrases = parser.getMultiVoicePhrases(partIndex)
    phrases.map(_.tempoBPM).toSet shouldBe Set(120.0)
    // 64 beats at 120 BPM are 32 s: six complete 5 second phrases (at 60 BPM there would be twelve)
    phrases should have size 6
  }

  it should "keep the tempo of a file with a single tempo" in {
    val file = MidiTestFiles.write(
      dir.resolve("constant.mid"),
      Seq(NoteTrack(channel = 0, pitches = Seq.tabulate(64)(60 + _ % 12))),
      tempos = Seq(0 -> 75.0))
    JMusicMIDIParser(file.toString).score.getTempo shouldBe 75.0 +- 1e-4
  }

  it should "keep jMusic's tempo for a file without tempo events" in {
    val file = MidiTestFiles.write(dir.resolve("no-tempo.mid"), Seq(NoteTrack(channel = 0, pitches = Seq.tabulate(8)(60 + _))))
    val score = new jm.music.data.Score()
    jm.util.Read.midi(score, file.toString)
    JMusicMIDIParser(file.toString).score.getTempo shouldBe score.getTempo
  }

  // trainingMIDIs/*.mid, with the tempo events of the file (read with javax.sound.midi): jMusic keeps the last event of
  // jdenver-anniessong.mid (60, 120, 100, then 16 BPM at the very end), blue.mid and i-always-get-lucky-with-you.mid
  // (a ritardando from 92 to 46 and from 80 to 47 BPM), and a garbled value for waterloo.mid (see MidiTempo)
  private val longestLasting = List(
    "jdenver-anniessong.mid" -> 120.0,
    "blue.mid" -> 92.0,
    "i-always-get-lucky-with-you.mid" -> 80.0,
    "waterloo.mid" -> 149.0, // a single tempo event of 402,680 microseconds per quarter note: 149.0017 BPM
    "cajun-fiddle.mid" -> 200.0) // a single tempo, unchanged by this fix

  longestLasting.foreach { case (resource, bpm) =>
    it should s"read $bpm BPM for $resource" in {
      val parser = parserOf(resource)
      parser.score.getTempo shouldBe bpm +- 0.01
      val partIndex = parser.getPartIndexByInstrument.values.flatten.min
      parser.getMultiVoicePhrases(partIndex).map(_.tempoBPM).toSet.foreach { (phraseBPM: Double) =>
        phraseBPM shouldBe bpm +- 0.01
      }
    }
  }
}
