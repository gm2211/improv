package midi

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import representation.{Chord, MusicalElement, Note, Phrase}

import java.io.File
import java.nio.file.{Files, Path}

/**
 * jMusic's times are doubles, so a note that starts on a bar line can start a nanosecond before it, and cutting the part
 * there used to leave a 1 ns note at the end of the phrase (#33).
 */
class JMusicMIDIParserSliversTest extends AnyFlatSpec with Matchers with BeforeAndAfterAll {
  private val dir: Path = Files.createTempDirectory("jmusic-parser-slivers-test")

  override def afterAll(): Unit = {
    def delete(file: File): Unit = {
      Option(file.listFiles).foreach(_.foreach(delete))
      file.delete()
    }
    delete(dir.toFile)
  }

  private def parserOf(resource: String): JMusicMIDIParser =
    JMusicMIDIParser(MidiTestFiles.copyResource(s"trainingMIDIs/$resource", dir.resolve(resource)).toString)

  private def voicesOf(phrase: Phrase): List[Phrase] =
    if (phrase.polyphony) phrase.musicalElements.asInstanceOf[List[Phrase]] else List(phrase)

  private def notesOf(element: MusicalElement): List[Note] = element match {
    case note: Note => List(note)
    case chord: Chord => chord.notes
    case _ => Nil
  }

  /** Every note of every voice of every phrase of the pitched parts of the file, with where it is (part, phrase). */
  private def notesWithPlace(parser: JMusicMIDIParser): List[(Note, String)] =
    for {
      part <- parser.getPartIndexByInstrument.values.flatten.toList.sorted
      (phrase, phraseIndex) <- parser.getMultiVoicePhrases(part).zipWithIndex
      voice <- voicesOf(phrase)
      note <- voice.musicalElements.flatMap(notesOf)
    } yield (note, s"part $part, phrase $phraseIndex")

  private def sliversOf(parser: JMusicMIDIParser): List[String] =
    notesWithPlace(parser).collect {
      case (note, place) if note.getDurationNS > 0 && note.getDurationNS < 1000 =>
        s"$place: pitch ${note.midiPitch} from ${note.getStartTimeNS} for ${note.getDurationNS} ns"
    }

  // 31 voices of all-my-exes-live-in-texas.mid (the first at part 1, phrase 1: pitch 44 from 3,999,999,999 ns for 1 ns)
  // and 331 of back-home-again.mid ended in a note of a nanosecond or two before the fix
  List("all-my-exes-live-in-texas.mid", "back-home-again.mid").foreach { resource =>
    "The phrases of " + resource should "have no note shorter than a microsecond where the part is cut on a bar line" in {
      sliversOf(parserOf(resource)) shouldBe empty
    }
  }
}
