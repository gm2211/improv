package training

import midi.{JMusicMIDIParser, MidiTestFiles}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import representation.{Chord, MusicalElement, Note, Phrase, Rest}

import java.nio.file.Files
import scala.concurrent.duration.MILLISECONDS

/**
 * Which phrases the case library takes: not silent throughout, not starting or ending in silence, silent for less than
 * 2/3 of their length. A polyphonic phrase is judged by the time in which no voice plays (#44); before, only the "all
 * rest" check applied to it, because its elements are voices and never rests.
 */
class DefaultMusicalCaseExtractorTest extends AnyFlatSpec with Matchers {
  private val accepts = (phrase: Phrase) => new DefaultMusicalCaseExtractor().getFilters(phrase)

  private val beat: BigInt = MILLISECONDS.toNanos(500) // a beat at 120 BPM

  // a voice from (start, length, isNote) in beats, laid end to end the way the parser builds them
  private def voice(elements: (Double, Double, Boolean)*): Phrase =
    new Phrase(elements.toList.zipWithIndex.map { case ((start, length, isNote), i) =>
      val startNS = (BigDecimal(beat) * start).toBigInt
      val lengthNS = (BigDecimal(beat) * length).toBigInt
      if (isNote) Note(midiPitch = 60 + i, startTimeNS = startNS, durationNS = lengthNS)
      else Rest(startTimeNS = startNS, durationNS = lengthNS)
    })

  private def note(start: Double, length: Double) = (start, length, true)
  private def rest(start: Double, length: Double) = (start, length, false)

  private def together(voices: Phrase*): Phrase = new Phrase(voices.toList, polyphony = true)

  "A monophonic phrase" should "be judged by its rests, as before" in {
    accepts(voice(note(0, 4), note(4, 4))) shouldBe true
    accepts(voice(rest(0, 1), note(1, 7))) shouldBe false // starts with a rest
    accepts(voice(note(0, 7), rest(7, 1))) shouldBe false // ends with a rest
    accepts(voice(note(0, 1), rest(1, 6), note(7, 1))) shouldBe false // 6 of 8 beats of rest
    accepts(voice(note(0, 2), rest(2, 4), note(6, 2))) shouldBe true // half of it
    accepts(voice(rest(0, 8))) shouldBe false
  }

  "A polyphonic phrase" should "be accepted when one voice waits for its entry while another plays" in {
    accepts(together(voice(note(0, 8)), voice(rest(0, 4), note(4, 4)))) shouldBe true
    // and when one voice stops early while another plays to the end
    accepts(together(voice(note(0, 8)), voice(note(0, 4), rest(4, 4)))) shouldBe true
    accepts(together(voice(note(0, 8)), voice(note(0, 4)))) shouldBe true
  }

  it should "be refused when no voice plays at its start" in {
    accepts(together(voice(rest(0, 1), note(1, 7)), voice(rest(0, 4), note(4, 4)))) shouldBe false
    // a single beat is enough, as for a monophonic phrase
    accepts(together(voice(rest(0, 1), note(1, 7)), voice(rest(0, 1), note(1, 7)))) shouldBe false
  }

  it should "be refused when no voice plays up to its end" in {
    accepts(together(voice(note(0, 7), rest(7, 1)), voice(note(0, 6), rest(6, 2)))) shouldBe false
    // a voice that has stopped is silent as well
    accepts(together(voice(note(0, 7), rest(7, 1)), voice(note(0, 4)))) shouldBe false
  }

  it should "be refused when no voice plays for 2/3 of it, and counted once where voices are silent together" in {
    // 6 of the 8 beats are silent in both voices
    accepts(together(voice(note(0, 1), rest(1, 6), note(7, 1)), voice(note(0, 1), rest(1, 6), note(7, 1)))) shouldBe false
    // each voice rests for 6 beats, but at different times: only 2 beats are silent for both
    accepts(together(voice(note(0, 1), rest(1, 6), note(7, 1)), voice(note(0, 3), rest(3, 2), note(5, 3)))) shouldBe true
    // silent exactly 2/3 of the time (beats 1 to 5 of 6) is refused, as for a monophonic phrase
    accepts(together(voice(note(0, 1), rest(1, 4), note(5, 1)), voice(note(0, 1), rest(1, 4), note(5, 1)))) shouldBe false
  }

  it should "be refused when every voice is all rest" in {
    accepts(together(voice(rest(0, 8)), voice(rest(0, 8)))) shouldBe false
  }

  it should "count a chord as playing" in {
    val chord = Chord(List(Note(midiPitch = 60, durationNS = beat * 8), Note(midiPitch = 64, durationNS = beat * 8)))
    accepts(together(new Phrase(List(chord)), voice(rest(0, 8)))) shouldBe true
  }

  it should "not take a gap of up to a microsecond between the notes of two voices for silence" in {
    val first = Note(midiPitch = 60, durationNS = beat * 4)
    val second = Note(midiPitch = 62, startTimeNS = beat * 4 + 1000, durationNS = beat * 4 - 1000)
    accepts(together(new Phrase(List(first)), new Phrase(List(Rest(durationNS = beat * 4 + 1000), second)))) shouldBe true
    // but notes that start more than a microsecond after the start of the phrase in every voice are a silent start
    val late = Note(midiPitch = 62, startTimeNS = 1001, durationNS = beat * 8)
    accepts(together(new Phrase(List(Rest(durationNS = 1001), late)), new Phrase(List(Rest(durationNS = 1001), late)))) shouldBe false
  }

  "A polyphonic phrase with a single voice" should "be judged exactly like that voice on its own" in {
    val voices = List(
      voice(note(0, 4), note(4, 4)),
      voice(rest(0, 1), note(1, 7)),
      voice(note(0, 7), rest(7, 1)),
      voice(note(0, 1), rest(1, 6), note(7, 1)),
      voice(note(0, 2), rest(2, 4), note(6, 2)),
      voice(rest(0, 8)))
    voices.foreach { v =>
      withClue(s"$v: ") { accepts(together(v)) shouldBe accepts(v) }
    }
  }

  // hey-good-lookin.mid has 34 phrases left with a single voice by a cut (Phrase.splitPolyphonic drops the voices with
  // nothing on one side of a cut but keeps polyphony = true) whose voice ends in a rest: the filter let them all through
  "The case library of hey-good-lookin.mid" should "refuse the phrases with a single voice that ends in a rest" in {
    val dir = Files.createTempDirectory("case-extractor-test")
    try {
      val file = MidiTestFiles.copyResource("trainingMIDIs/hey-good-lookin.mid", dir.resolve("hey-good-lookin.mid"))
      val parser = JMusicMIDIParser(file.toString)
      val phrases = parser.getPartIndexByInstrument.values.flatten.toList.sorted.flatMap(parser.getMultiVoicePhrases)
      val singleVoice = phrases.filter(p => p.polyphony && p.musicalElements.size == 1)
      val endingInRest = singleVoice.filter(_.musicalElements.head.asInstanceOf[Phrase].last.isInstanceOf[Rest])

      endingInRest should have size 34
      endingInRest.foreach(accepts(_) shouldBe false)
      singleVoice.foreach(p => accepts(p) shouldBe accepts(p.musicalElements.head.asInstanceOf[Phrase]))
    } finally {
      Option(dir.toFile.listFiles).foreach(_.foreach(_.delete()))
      dir.toFile.delete()
    }
  }
}
