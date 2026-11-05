package midi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import representation.KeySignature.{Major, Minor}
import representation.{KeySignature, KeySignatureChange}

import javax.sound.midi.{MetaMessage, MidiEvent, Sequence}

class MidiKeySignatureTest extends AnyFlatSpec with Matchers {
  private val ticks = MidiTestFiles.TICKS_PER_QUARTER

  private def sequence(lengthBeats: Int, tracks: Seq[MidiEvent]*): Sequence = {
    val sequence = new Sequence(Sequence.PPQ, ticks)
    tracks.foreach { events =>
      val track = sequence.createTrack()
      events.foreach(track.add)
    }
    val last = sequence.createTrack()
    last.add(new MidiEvent(new MetaMessage(0x01, Array[Byte](0), 1), lengthBeats.toLong * ticks)) // a text event at the end
    sequence
  }

  "The key signature events of a sequence" should "be read from every track, in time order, with their start in beats" in {
    val seq = sequence(64,
      Seq(MidiTestFiles.keySignatureEvent(0, -2, minor = false), MidiTestFiles.keySignatureEvent(48, 0, minor = true)),
      Seq(MidiTestFiles.keySignatureEvent(32, 3, minor = false)))

    MidiKeySignature.read(seq) shouldBe List(
      KeySignatureChange(BigDecimal(0), KeySignature(flats = 2, sharps = 0, Major)),
      KeySignatureChange(BigDecimal(32), KeySignature(flats = 0, sharps = 3, Major)),
      KeySignatureChange(BigDecimal(48), KeySignature(flats = 0, sharps = 0, Minor)))
  }

  it should "skip malformed events and read nothing from a sequence in SMPTE time" in {
    val malformed = Seq(
      new MidiEvent(new MetaMessage(0x59, Array[Byte](2), 1), 0), // no mode
      new MidiEvent(new MetaMessage(0x59, Array[Byte](9, 0), 2), 0), // 9 sharps
      new MidiEvent(new MetaMessage(0x59, Array[Byte](-8, 0), 2), 0), // 8 flats
      new MidiEvent(new MetaMessage(0x59, Array[Byte](1, 2), 2), 0), // mode 2
      MidiTestFiles.keySignatureEvent(4, -7, minor = true))
    MidiKeySignature.read(sequence(8, malformed)) shouldBe
      List(KeySignatureChange(BigDecimal(4), KeySignature(flats = 7, sharps = 0, Minor)))

    val smpte = new Sequence(Sequence.SMPTE_25, 40)
    smpte.createTrack().add(MidiTestFiles.keySignatureEvent(0, 2, minor = false))
    MidiKeySignature.read(smpte) shouldBe Nil
  }

  "The key that lasts longest in a sequence" should "count up to the end of the sequence, not stop at the last event" in {
    // B flat for 24 beats, then A for the remaining 40: jMusic would keep A (the last event) either way
    MidiKeySignature.longestLasting(sequence(64,
      Seq(MidiTestFiles.keySignatureEvent(0, -2, minor = false), MidiTestFiles.keySignatureEvent(24, 3, minor = false)))) shouldBe
      Some(KeySignature(flats = 0, sharps = 3, Major))
    // B flat for 40 beats, then A for 24: B flat, not the last event
    MidiKeySignature.longestLasting(sequence(64,
      Seq(MidiTestFiles.keySignatureEvent(0, -2, minor = false), MidiTestFiles.keySignatureEvent(40, 3, minor = false)))) shouldBe
      Some(KeySignature(flats = 2, sharps = 0, Major))
    MidiKeySignature.longestLasting(sequence(64)) shouldBe None
  }

  it should "be None for a file that cannot be read" in {
    MidiKeySignature.longestLasting("/no/such/dir/missing.mid") shouldBe None
    MidiKeySignature.read("/no/such/dir/missing.mid") shouldBe Nil
  }
}
