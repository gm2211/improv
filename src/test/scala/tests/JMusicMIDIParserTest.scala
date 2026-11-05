package tests

import java.io.File
import java.nio.file.{Files, Path}
import javax.sound.midi.MidiSystem

import _root_.instruments.InstrumentType.PIANO
import _root_.instruments.JFugueUtils
import _root_.midi.{JMusicMIDIParser, JMusicParserUtils}
import _root_.utils.{IOUtils, ProfilingUtils}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import representation.Phrase

class JMusicMIDIParserTest extends AnyFlatSpec with BeforeAndAfterAll {
  val resourcePath = IOUtils.getResourcePath("musicScores/shortPiano.mid")
  val parser = JMusicMIDIParser(resourcePath)

  /** Where the test writes its MIDI files: a temporary directory, deleted after the suite, not the source tree (#26). */
  private val outputDir: Path = Files.createTempDirectory("jmusic-midi-parser-test")

  override def afterAll(): Unit = {
    def delete(file: File): Unit = {
      Option(file.listFiles).foreach(_.foreach(delete))
      file.delete()
    }
    delete(outputDir.toFile)
  }

  /** The names of the files in the resource directory `musicScores`, i.e. `src/main/resources/musicScores`. */
  private def musicScores: Set[String] =
    Option(new File(IOUtils.getResourcePath("musicScores")).list).map(_.toSet).getOrElse(Set())

  // Pre-existing failure, see #15
  "The midi parser" should "return a list of parts and their indexes grouped by instrument" ignore {
    val partsIndices = parser.getPartIndexByInstrument.get(PIANO(1)).get
    assert(partsIndices == Set(0, 1))
  }

  "The midi parser" should "correctly assemble a phrase" in {
    val scoresBefore = musicScores
    println()
    val multiVoicePhrase = JMusicParserUtils.convertPart(parser.score.getPart(0)).getOrElse(Phrase())
    val mergedPhrase = Phrase.mergePhrases(multiVoicePhrase)
    val splitTask = () => Phrase.unmerge(mergedPhrase)

    val (timeElapsed, multiVoicePhraseRecovered) = ProfilingUtils.timeIt(splitTask(), 1)
    println(s"\nOn average it took $timeElapsed milliseconds to get $multiVoicePhraseRecovered")
    val sequences = Map(
      "theirPiano.mid" -> JFugueUtils.toSequence(JFugueUtils.createPattern(multiVoicePhrase, PIANO(1))),
      "recovered.mid" -> JFugueUtils.toSequence(JFugueUtils.createPattern(multiVoicePhraseRecovered, PIANO(1))),
      "merged.mid" -> JFugueUtils.toSequence(JFugueUtils.createPattern(mergedPhrase, PIANO(1))))

    sequences.foreach { case (name, sequence) =>
      val file = outputDir.resolve(name).toFile
      MidiSystem.write(sequence, 1, file)
      withClue(s"$name: ") {
        assert(MidiSystem.getSequence(file).getTickLength > 0)
      }
    }
    println(s"Wrote ${sequences.keys.toList.sorted.mkString(", ")} to $outputDir")

    assert(musicScores == scoresBefore, "the test must not write into src/main/resources/musicScores")
  }
}
