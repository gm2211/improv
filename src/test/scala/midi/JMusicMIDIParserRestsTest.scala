package midi

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import representation.{Note, Phrase, Rest}
import training.DefaultMusicalCaseExtractor

import java.io.File
import java.nio.file.{Files, Path}

/**
 * jMusic's times are doubles, so two notes that touch in the file come out a few nanoseconds apart, and the parser used
 * to turn that gap into a rest of 1 to 99 ns (#39). A phrase whose last note was followed by one was taken for a phrase
 * that ends in a rest and left out of the case library.
 */
class JMusicMIDIParserRestsTest extends AnyFlatSpec with Matchers with BeforeAndAfterAll {
  private val dir: Path = Files.createTempDirectory("jmusic-parser-rests-test")

  override def afterAll(): Unit = {
    def delete(file: File): Unit = {
      Option(file.listFiles).foreach(_.foreach(delete))
      file.delete()
    }
    delete(dir.toFile)
  }

  private val MICROSECOND_NS = 1000

  private def parserOf(resource: String): JMusicMIDIParser =
    JMusicMIDIParser(MidiTestFiles.copyResource(s"trainingMIDIs/$resource", dir.resolve(resource)).toString)

  private def voicesOf(phrase: Phrase): List[Phrase] =
    if (phrase.polyphony) phrase.musicalElements.asInstanceOf[List[Phrase]] else List(phrase)

  /** (kind, start, duration) of the elements, to compare them with what the file says. */
  private def shapeOf(voice: Phrase): List[(String, BigInt, BigInt)] = voice.musicalElements.map {
    case n: Note => (s"note ${n.midiPitch}", n.getStartTimeNS, n.getDurationNS)
    case r: Rest => ("rest", r.getStartTimeNS, r.getDurationNS)
    case other => (other.getClass.getSimpleName, other.getStartTimeNS, other.getDurationNS)
  }

  /** Every rest of a microsecond or less in every voice of every phrase (library and merged) of the pitched parts, with where it is. */
  private def tinyRests(parser: JMusicMIDIParser): List[String] =
    for {
      part <- parser.getPartIndexByInstrument.values.flatten.toList.sorted
      (phrase, phraseIndex) <- parser.getMultiVoicePhrases(part).zipWithIndex ++ parser.getPhrases(part).zipWithIndex
      voice <- voicesOf(phrase)
      rest <- voice.musicalElements.collect { case r: Rest if r.getDurationNS <= MICROSECOND_NS => r }
    } yield s"part $part, phrase $phraseIndex: rest of ${rest.getDurationNS} ns at ${rest.getStartTimeNS}"

  // 839 rests of 1 to 99 ns in boot-scootin-boogie.mid, the most of the 76 files (6,087 in all); flowers-on-the-wall.mid has
  // the most voices that ended in one (97 of the 549)
  List("boot-scootin-boogie.mid", "flowers-on-the-wall.mid", "hey-good-lookin.mid").foreach { resource =>
    "The phrases of " + resource should "have no rest of a microsecond or less" in {
      tinyRests(parserOf(resource)) shouldBe empty
    }
  }

  "hey-good-lookin.mid" should "keep a note where the file puts it when the note before it ends a nanosecond or two earlier" in {
    // part 0, phrase 11: a note that ends at 1,249,999,998 ns and the next one that starts at 1,250,000,000 (a rest of 1 ns
    // between them, and another one at the end of the phrase, before the change); the notes are not moved
    val voice = voicesOf(parserOf("hey-good-lookin.mid").getMultiVoicePhrases(0)(11)).head

    shapeOf(voice) shouldBe List(
      ("note 73", BigInt(0), BigInt(312500000)),
      ("note 71", BigInt(312500000), BigInt(416666666)),
      ("rest", BigInt(729166666), BigInt(104166666)), // a real rest: it is still there
      ("note 76", BigInt(833333333), BigInt(416666665)),
      ("note 78", BigInt(1250000000), BigInt(1249999998)))
  }

  it should "not end a phrase in a rest that is only noise" in {
    // part 0, phrase 8 ended in a rest of 1 ns after its last note
    val voice = voicesOf(parserOf("hey-good-lookin.mid").getMultiVoicePhrases(0)(8)).head

    voice.musicalElements.last shouldBe a[Note]
    voice.musicalElements.last.getEndTimeNS shouldBe BigInt(2499999998L)
  }

  "The case library" should "take a phrase that ends in a note when a rest of a few nanoseconds used to follow it" in {
    // flowers-on-the-wall.mid, part 3: phrases 15 and 16 are one note of 2.77 s that a rest of 1 ns and of 6 ns followed,
    // and phrase 18 two notes and a rest of 1 ns
    val parser = parserOf("flowers-on-the-wall.mid")
    val extractor = new DefaultMusicalCaseExtractor()

    for (index <- List(15, 16, 18)) {
      val phrase = parser.getMultiVoicePhrases(3)(index)

      withClue(s"phrase $index: ") {
        phrase.polyphony shouldBe false
        phrase.last shouldBe a[Note]
        extractor.getFilters(phrase) shouldBe true
      }
    }
  }

  // jMusic's own rests, and the first note of a phrase that does not start at time 0, are not noise
  "A jMusic phrase" should "begin with a rest when it starts late, and have a rest where its notes are really apart" in {
    val jmPhrase = new jm.music.data.Phrase(2.0) // 2 beats = 1 second at 120 BPM
    jmPhrase.addNote(new jm.music.data.Note(60, 1.0))
    jmPhrase.addNote(new jm.music.data.Rest(1.0))
    jmPhrase.addNote(new jm.music.data.Note(62, 1.0))

    val phrase = JMusicParserUtils.convertPhrase(jmPhrase)

    shapeOf(phrase) shouldBe List(
      ("rest", BigInt(0), BigInt(1000000000)),
      ("note 60", BigInt(1000000000), BigInt(500000000)),
      ("rest", BigInt(1500000000), BigInt(500000000)),
      ("note 62", BigInt(2000000000), BigInt(500000000)))
  }

  it should "have no rest between notes that touch" in {
    val jmPhrase = new jm.music.data.Phrase(0.0)
    List(60, 62, 64, 65, 67).foreach(pitch => jmPhrase.addNote(new jm.music.data.Note(pitch, 0.25)))

    val phrase = JMusicParserUtils.convertPhrase(jmPhrase)

    phrase.musicalElements.collect { case n: Note => n.midiPitch } shouldBe List(60, 62, 64, 65, 67)
    phrase.musicalElements.exists(_.isInstanceOf[Rest]) shouldBe false
  }
}
