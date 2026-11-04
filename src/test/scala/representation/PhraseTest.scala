package representation

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import representation.NoteName._

import scala.concurrent.duration.SECONDS

class PhraseTest extends AnyFlatSpec with Matchers {

  "Notes across the split time" should "be split among the two resulting phrases" in {
    val p = Phrase()
      .withMusicalElements(
        Phrase().withMusicalElements(Note(A).withStartTime(0, SECONDS).withDuration(1, SECONDS),
                                     Note(D).withStartTime(1, SECONDS).withDuration(2, SECONDS),
                                     Note(NoteName.F).withStartTime(3, SECONDS).withDuration(1, SECONDS)),
        Phrase().withMusicalElements(Note(B).withStartTime(0, SECONDS).withDuration(2, SECONDS),
                                     Note(E).withStartTime(2, SECONDS).withDuration(2, SECONDS)),
        Phrase().withMusicalElements(Note(C).withStartTime(0, SECONDS).withDuration(4, SECONDS))
      )
      .withPolyphony(polyphony = true).get
    val (p1, p2) = Phrase.split(p, SECONDS.toNanos(1))
    val bSplitP1 = p1.get.musicalElements(1).asInstanceOf[Phrase].musicalElements.head.asInstanceOf[Note]
    val bSplitP2 = p2.get.musicalElements(1).asInstanceOf[Phrase].musicalElements.head.asInstanceOf[Note]

    assert(bSplitP1.name.equals(B))
    assert(bSplitP1.getDuration(SECONDS) == BigInt(1))
    assert(bSplitP1.getStartTime(SECONDS) == BigInt(0))

    assert(bSplitP2.name.equals(B))
    assert(bSplitP2.getDuration(SECONDS) == BigInt(1))
    assert(bSplitP2.getStartTime(SECONDS) == BigInt(0))
  }

  private def noteAt(pitch: Int, startSeconds: Int, seconds: Int): Note =
    Note(midiPitch = pitch, startTimeNS = SECONDS.toNanos(startSeconds), durationNS = SECONDS.toNanos(seconds))

  /** Two voices: 72 then 74 for a second each, and 48 for two seconds, all at the given tempo. */
  private def twoVoices(tempoBPM: Double): Phrase = {
    val high = new Phrase(List(noteAt(72, 0, 1), noteAt(74, 1, 1)), tempoBPM = tempoBPM)
    val low = new Phrase(List(noteAt(48, 0, 2)), tempoBPM = tempoBPM)
    new Phrase(List(high, low), polyphony = true, tempoBPM = tempoBPM)
  }

  "Merging the voices of a phrase" should "keep the tempo of the phrase (#34)" in {
    val merged = Phrase.mergePhrases(twoVoices(90))

    merged.tempoBPM shouldBe 90.0
    merged.polyphony shouldBe false
  }

  it should "give the same notes whatever the tempo: the times are nanoseconds" in {
    for (tempo <- List(60.0, 90.0, 120.0, 200.0)) {
      val merged = Phrase.mergePhrases(twoVoices(tempo))
      val chords = merged.musicalElements.collect { case chord: Chord => chord }

      withClue(s"at $tempo BPM: ") {
        chords should have size 2
        merged.musicalElements should have size 2
        chords.map(c => (c.getStartTimeNS, c.getDurationNS, c.notes.map(_.midiPitch).sorted)) shouldBe List(
          (BigInt(0), BigInt(SECONDS.toNanos(1)), List(48, 72)),
          (BigInt(SECONDS.toNanos(1)), BigInt(SECONDS.toNanos(1)), List(48, 74)))
        merged.getDurationNS shouldBe BigInt(SECONDS.toNanos(2))
      }
    }
  }

  it should "return a phrase with one voice as it is" in {
    val melody = new Phrase(List(noteAt(60, 0, 1)), tempoBPM = 75)

    Phrase.mergePhrases(melody) should be theSameInstanceAs melody
    Phrase.mergePhrases(melody).tempoBPM shouldBe 75.0
  }

  it should "give a list of phrases the tempo of the first one, and the default tempo when there is none" in {
    val voices = twoVoices(90).musicalElements.asInstanceOf[List[Phrase]]

    Phrase.mergePhrases(voices).tempoBPM shouldBe 90.0
    Phrase.mergePhrases(List[Phrase]()).tempoBPM shouldBe MusicalElement.DEFAULT_TEMPO_BPM
  }

  it should "not change the note times when the voices have different tempos: they are nanoseconds" in {
    val fast = new Phrase(List(noteAt(72, 0, 1), noteAt(74, 1, 1)), tempoBPM = 150)
    val slow = new Phrase(List(noteAt(48, 0, 2)), tempoBPM = 90)

    val merged = Phrase.mergePhrases(List(fast, slow))

    merged.tempoBPM shouldBe 150.0
    Phrase.mergePhrases(List(slow, fast)).tempoBPM shouldBe 90.0
    merged.getDurationNS shouldBe BigInt(SECONDS.toNanos(2))
    merged.musicalElements should have size 2
  }
}
