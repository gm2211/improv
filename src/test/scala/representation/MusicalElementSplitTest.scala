package representation

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Cutting an element: a cut that falls within the tolerance of one of its ends moves to that end (#33). jMusic's times
 * are doubles that are truncated to whole nanoseconds, so a note that starts on a cut can start a nanosecond before it.
 */
class MusicalElementSplitTest extends AnyFlatSpec with Matchers {
  private val cut = BigInt(4000000000L)
  private val tolerance = MusicalElement.SPLIT_TOLERANCE_NS

  private def note(startNS: BigInt, durationNS: BigInt) = Note(midiPitch = 60, startTimeNS = startNS, durationNS = durationNS)

  "Cutting an element" should "cut a note that the cut goes through into the part before and the part after" in {
    val (left, right) = MusicalElement.split(note(cut - 500000000, 1000000000), cut)

    left shouldBe Some(note(cut - 500000000, 500000000))
    right shouldBe Some(note(cut, 500000000))
  }

  it should "leave a note whole on the right when it starts less than the tolerance before the cut, whatever its length" in {
    for (before <- List[BigInt](1, 2, 3, 500, tolerance - 1); duration <- List[BigInt](tolerance, 500000000, 3000000000L)) {
      val elem = note(cut - before, duration + before)
      withClue(s"starting $before ns before the cut, lasting ${duration + before} ns: ") {
        MusicalElement.split(elem, cut) shouldBe ((None, Some(elem)))
      }
    }
  }

  it should "leave a note whole on the left when it ends less than the tolerance after the cut" in {
    for (after <- List[BigInt](1, 2, 3, 500, tolerance - 1); duration <- List[BigInt](tolerance, 500000000, 3000000000L)) {
      val elem = note(cut - duration, duration + after)
      withClue(s"ending $after ns after the cut, lasting ${duration + after} ns: ") {
        MusicalElement.split(elem, cut) shouldBe ((Some(elem), None))
      }
    }
  }

  it should "still cut a note when the part before or after the cut is as long as the tolerance" in {
    MusicalElement.split(note(cut - tolerance, 1000000000), cut) shouldBe
      ((Some(note(cut - tolerance, tolerance)), Some(note(cut, BigInt(1000000000) - tolerance))))
    MusicalElement.split(note(cut - 1000000000, 1000000000 + tolerance), cut) shouldBe
      ((Some(note(cut - 1000000000, 1000000000)), Some(note(cut, tolerance))))
  }

  it should "give a note that starts or ends exactly on the cut to the side it lies on, as before" in {
    val after = note(cut, 1000000000)
    val before = note(cut - 1000000000, 1000000000)

    MusicalElement.split(after, cut) shouldBe ((None, Some(after)))
    MusicalElement.split(before, cut) shouldBe ((Some(before), None))
  }

  it should "give a note that lies wholly before or after the cut to that side, as before" in {
    val earlier = note(cut - 2000000000, 1000000000)
    val later = note(cut + 1000000000, 1000000000)

    MusicalElement.split(earlier, cut) shouldBe ((Some(earlier), None))
    MusicalElement.split(later, cut) shouldBe ((None, Some(later)))
  }

  it should "treat a rest and a chord like a note" in {
    val rest = Rest(startTimeNS = cut - 1, durationNS = 2000000000)
    val chord = Chord(List(note(cut - 1000000000, 1000000001), note(cut - 1000000000, 1000000001)))

    MusicalElement.split(rest, cut) shouldBe ((None, Some(rest)))
    MusicalElement.split(chord, cut) shouldBe ((Some(chord), None))
  }

  it should "cut exactly where it is told to when the tolerance is zero" in {
    val elem = note(cut - 1, 1000000000)

    MusicalElement.split(elem, cut, toleranceNS = 0) shouldBe ((Some(note(cut - 1, 1)), Some(note(cut, 1000000000 - 1))))
  }

  it should "cut an element that is shorter than twice the tolerance as before" in {
    val elem = note(cut - 700, 1500)

    MusicalElement.split(elem, cut) shouldBe ((Some(note(cut - 700, 700)), Some(note(cut, 800))))
  }

  it should "use the tolerance it is given" in {
    val elem = note(cut - 5000, 1000000000)

    MusicalElement.split(elem, cut) shouldBe ((Some(note(cut - 5000, 5000)), Some(note(cut, 1000000000 - 5000))))
    MusicalElement.split(elem, cut, toleranceNS = 10000) shouldBe ((None, Some(elem)))
  }
}
