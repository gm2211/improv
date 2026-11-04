package representation

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import representation.KeySignature.{Major, Minor}

/** Which key holds where in a piece, from its key signature events (#41). */
class KeySignatureChangeTest extends AnyFlatSpec with Matchers {
  private val bFlat = KeySignature(flats = 2, sharps = 0, Major)
  private val aMajor = KeySignature(flats = 0, sharps = 3, Major)
  private val fMajor = KeySignature(flats = 1, sharps = 0, Major)
  private val cMajor = KeySignatureChange.Default

  private def change(beat: Int, key: KeySignature) = KeySignatureChange(BigDecimal(beat), key)

  "A MIDI key signature" should "be read as sharps, flats and a quality" in {
    KeySignatureChange.fromMidi(3, minor = false) shouldBe aMajor
    KeySignatureChange.fromMidi(-2, minor = false) shouldBe bFlat
    KeySignatureChange.fromMidi(-3, minor = true) shouldBe KeySignature(flats = 3, sharps = 0, Minor)
    KeySignatureChange.fromMidi(0, minor = false) shouldBe cMajor
    cMajor shouldBe KeySignature(flats = 0, sharps = 0, Major)
  }

  "The sections of a piece" should "start in C major until the first event, and keep the last of simultaneous events" in {
    KeySignatureChange.sections(Nil) shouldBe Vector(change(0, cMajor))
    KeySignatureChange.sections(List(change(8, bFlat))) shouldBe Vector(change(0, cMajor), change(8, bFlat))
    KeySignatureChange.sections(List(change(0, bFlat), change(0, aMajor))) shouldBe Vector(change(0, aMajor))
    // unsorted, a repeated key is no change
    KeySignatureChange.sections(List(change(32, aMajor), change(0, bFlat), change(16, bFlat))) shouldBe
      Vector(change(0, bFlat), change(32, aMajor))
  }

  "The key at a beat" should "be the one of the last change at or before it" in {
    val changes = List(change(0, bFlat), change(32, aMajor))
    KeySignatureChange.at(changes, 0) shouldBe bFlat
    KeySignatureChange.at(changes, BigDecimal("31.99")) shouldBe bFlat
    KeySignatureChange.at(changes, 32) shouldBe aMajor
    KeySignatureChange.at(Nil, 100) shouldBe cMajor
  }

  "The key of a stretch" should "be the one that lasts longest in it" in {
    val changes = List(change(0, bFlat), change(32, aMajor), change(64, fMajor))
    KeySignatureChange.longestIn(changes, 0, 8) shouldBe bFlat
    KeySignatureChange.longestIn(changes, 32, 40) shouldBe aMajor // a change on the first beat of the stretch
    KeySignatureChange.longestIn(changes, 24, 32) shouldBe bFlat // a change on the end of the stretch is not in it
    KeySignatureChange.longestIn(changes, 26, 34) shouldBe bFlat // 6 beats of B flat, 2 of A
    KeySignatureChange.longestIn(changes, 30, 38) shouldBe aMajor // 2 beats of B flat, 6 of A
    KeySignatureChange.longestIn(changes, 100, 108) shouldBe fMajor // after the last change
    // over three keys: 1 + 32 + 1 beats
    KeySignatureChange.longestIn(changes, 31, 65) shouldBe aMajor
  }

  it should "add up the stretches of one key, and give a tie to the key that comes first" in {
    // B flat 0-4 and 8-12, A 4-8 and then from 12: 8 beats of B flat against 4 of A in 0-12
    val changes = List(change(0, bFlat), change(4, aMajor), change(8, bFlat), change(12, aMajor))
    KeySignatureChange.longestIn(changes, 0, 12) shouldBe bFlat
    // B flat 2-4 and 8-10, A 4-8: 4 beats each, and B flat comes first
    KeySignatureChange.longestIn(changes, 2, 10) shouldBe bFlat
    KeySignatureChange.longestIn(List(change(0, bFlat), change(4, aMajor)), 0, 8) shouldBe bFlat
    KeySignatureChange.longestIn(List(change(0, aMajor), change(4, bFlat)), 0, 8) shouldBe aMajor
  }

  it should "be C major before the first event and without events, and the key at its start when it has no length" in {
    KeySignatureChange.longestIn(List(change(16, aMajor)), 0, 8) shouldBe cMajor
    KeySignatureChange.longestIn(List(change(4, aMajor)), 0, 8) shouldBe cMajor // a tie: C major comes first
    KeySignatureChange.longestIn(List(change(3, aMajor)), 0, 8) shouldBe aMajor
    KeySignatureChange.longestIn(Nil, 0, 8) shouldBe cMajor
    KeySignatureChange.longestIn(List(change(0, bFlat), change(8, aMajor)), 8, 8) shouldBe aMajor
  }
}
