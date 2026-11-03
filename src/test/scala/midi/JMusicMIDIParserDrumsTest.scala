package midi

import instruments.InstrumentType.PIANO
import midi.MidiTestFiles.NoteTrack
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import training.MusicalCaseExtractors

import java.io.File
import java.nio.file.{Files, Path}

/** Drum parts (MIDI channel 10, index 9) must not reach the case library as pitched parts (#22). */
class JMusicMIDIParserDrumsTest extends AnyFlatSpec with Matchers with BeforeAndAfterAll {
  private val dir: Path = Files.createTempDirectory("jmusic-parser-drums-test")

  override def afterAll(): Unit = {
    def delete(file: File): Unit = {
      Option(file.listFiles).foreach(_.foreach(delete))
      file.delete()
    }
    delete(dir.toFile)
  }

  // A melody on channel 0 without a program change (jMusic: program 0, PIANO) and a drum track on channel 9 with a
  // program change to 40 (jMusic: the drum part gets program 40, STRINGS), 64 quarter notes each
  private lazy val synthetic: String = MidiTestFiles.write(
    dir.resolve("melody-and-drums.mid"),
    Seq(
      NoteTrack(channel = 0, pitches = Seq.tabulate(64)(60 + _ % 12)),
      NoteTrack(channel = 9, pitches = Seq.tabulate(64)(i => if (i % 2 == 0) 36 else 38), program = Some(40)))).toString

  private lazy val cajunFiddle: String =
    MidiTestFiles.copyResource("trainingMIDIs/cajun-fiddle.mid", dir.resolve("cajun-fiddle.mid")).toString

  "The midi parser" should "not classify the drum part of a file by instrument" in {
    val parser = JMusicMIDIParser(synthetic)
    val parts = parser.score.getPartArray
    // jMusic does read the drum track as a part of the score, with the program of its track
    val drumIndex = parts.indexWhere(_.getChannel == 9)
    val melodyIndex = parts.indexWhere(_.getChannel == 0)
    drumIndex should be >= 0
    melodyIndex should be >= 0
    parts(drumIndex).getInstrument shouldBe 40

    parser.getPartIndexByInstrument.keySet shouldBe Set(PIANO(1))
    parser.getPartIndexByInstrument(PIANO(1)) shouldBe Set(melodyIndex)
    parser.getInstrumentsCounts shouldBe Map(PIANO(1) -> 1)
  }

  it should "still read a drum part when it is asked for by its index" in {
    val parser = JMusicMIDIParser(synthetic)
    val drumIndex = parser.score.getPartArray.indexWhere(_.getChannel == 9)
    parser.getMultiVoicePhrases(drumIndex) should not be empty
  }

  it should "give the case library cases from the melody part only" in {
    val cases = MusicalCaseExtractors.getDefault().getCases(synthetic)

    cases should not be empty
    // a drum track would add STRINGS cases (its program is 40) and a case from the drum part to the melody and back
    cases.flatMap { case (problem, solution) => List(problem, solution) }.map(_.instrumentType).toSet shouldBe Set(PIANO(1))
    val parser = JMusicMIDIParser(synthetic)
    val melodyIndex = parser.score.getPartArray.indexWhere(_.getChannel == 0)
    cases should have size parser.getMultiVoicePhrases(melodyIndex).size - 1
  }

  it should "leave the drum parts out of a real file" in {
    // cajun-fiddle.mid: a fiddle on channel 0 (program 110, ETHNIC), one on channel 1 (no program change, PIANO) and two
    // drum tracks on channel 9 (four parts in jMusic's score; the first has program 9, so it was CHROMATIC_PERCUSSION)
    val parser = JMusicMIDIParser(cajunFiddle)
    val parts = parser.score.getPartArray
    parts.map(_.getChannel).toList.sorted shouldBe List(0, 1, 9, 9)

    val classified = parser.getPartIndexByInstrument.values.flatten.toList
    classified.map(parts(_).getChannel).sorted shouldBe List(0, 1)
    parser.getInstrumentsCounts.values.sum shouldBe 2

    val cases = MusicalCaseExtractors.getDefault().getCases(cajunFiddle)
    cases.flatMap { case (problem, solution) => List(problem, solution) }
      .map(_.instrumentType.getClass.getSimpleName).toSet shouldBe Set("ETHNIC", "PIANO")
  }
}
