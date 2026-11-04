package instruments

import instruments.InstrumentType.PIANO
import javax.sound.midi.ShortMessage
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import representation.{MusicalElement, Note, NoteName, Phrase}

class JFugueUtilsTest extends AnyFlatSpec with Matchers {
  private val d1 = MusicalElement.fromBPM(0.5)
  private val d2 = MusicalElement.fromBPM(1)

  /**
   * Three voices of two notes each: half a beat, then a beat that starts where the first note ends (at 120 BPM, the
   * default tempo). The second note needs its start time: a `Note` starts at 0 unless it is given one, which is why this
   * test used to see every note at `@0.0` (#15). The parser gives every note its start time.
   */
  private val polyphonicPhrase = {
    val voices = List(
      List(new Note(NoteName.A, durationNS = d1), new Note(NoteName.D, durationNS = d2, startTimeNS = d1)),
      List(new Note(NoteName.B, durationNS = d1), new Note(NoteName.E, durationNS = d2, startTimeNS = d1)),
      List(new Note(NoteName.C, durationNS = d1), new Note(NoteName.F, durationNS = d2, startTimeNS = d1))
    ).map(notes => new Phrase(notes))
    new Phrase(musicalElements = voices, polyphony = true)
  }

  "A polyphonic phrase" should "be converted into a pattern that uses @<time_val>" in {
    // T480: createPattern sets 4 times the tempo of the phrase, so a JFugue duration in whole notes (h = 0.5, w = 1) and
    // an @ time are the phrase's beats; a70d0 is the attack and decay velocity of each note (Loudness 70, off velocity 0)
    val pattern = JFugueUtils.createPattern(polyphonicPhrase, PIANO(1))
    pattern.toString shouldBe
      "T480 I[Piano] @0.0 A4ha70d0 @0.0 B4ha70d0 @0.0 C4ha70d0 @0.5 D4wa70d0 @0.5 E4wa70d0 @0.5 F4wa70d0"
  }

  it should "start each note of the MIDI sequence where the previous note of its voice ends" in {
    val sequence = JFugueUtils.toSequence(JFugueUtils.createPattern(polyphonicPhrase, PIANO(1)))
    val events = sequence.getTracks.toList.flatMap(track => (0 until track.size).map(track.get))
    val noteEvents = events.flatMap { event =>
      event.getMessage match {
        case message: ShortMessage if message.getCommand == ShortMessage.NOTE_ON && message.getData2 > 0 =>
          Some((message.getData1, "on", event.getTick))
        case message: ShortMessage if message.getCommand == ShortMessage.NOTE_OFF ||
            (message.getCommand == ShortMessage.NOTE_ON && message.getData2 == 0) =>
          Some((message.getData1, "off", event.getTick))
        case _ => None
      }
    }
    def tick(pitch: Int, kind: String): Long =
      noteEvents.collectFirst { case (`pitch`, `kind`, t) => t }.getOrElse(fail(s"no note $kind for pitch $pitch"))

    // A4 B4 C4, then D4 E4 F4, as MIDI pitches (JFugue's octave numbers: C5 is middle C, 60)
    def pitch(name: String): Int = new org.jfugue.theory.Note(name).getValue.toInt
    val firstNotes = List("A4", "B4", "C4").map(pitch)
    val secondNotes = List("D4", "E4", "F4").map(pitch)
    firstNotes.map(tick(_, "on")).distinct shouldBe List(0L)
    val endOfFirstNotes = firstNotes.map(tick(_, "off")).distinct
    endOfFirstNotes should have size 1
    endOfFirstNotes.head should be > 0L
    secondNotes.map(tick(_, "on")).distinct shouldBe endOfFirstNotes
    // the second notes are twice as long as the first ones
    secondNotes.map(pitch => tick(pitch, "off") - tick(pitch, "on")).distinct shouldBe List(2 * endOfFirstNotes.head)
  }
}
