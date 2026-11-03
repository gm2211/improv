package midi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import representation.{TimeSignature, TimeSignatureChange}

import java.io.File
import java.nio.file.Files
import javax.sound.midi.{MetaMessage, MidiEvent, Sequence}

/** The time signature events of a MIDI file, as bars start from them (#27). */
class MidiTimeSignatureTest extends AnyFlatSpec with Matchers {
  private val resolution = 480

  private def sequence(division: Float = Sequence.PPQ, ticksPerFrame: Int = resolution): Sequence =
    new Sequence(division, ticksPerFrame)

  private def timeSignature(tick: Long, data: Int*): MidiEvent =
    new MidiEvent(new MetaMessage(0x58, data.map(_.toByte).toArray, data.size), tick)

  private def change(beat: BigDecimal, numerator: Int, denominator: Int) =
    TimeSignatureChange(beat, TimeSignature(numerator, denominator))

  "The time signatures of a sequence" should "come with their start in quarter notes" in {
    val seq = sequence()
    val track = seq.createTrack()
    track.add(timeSignature(0, 4, 2, 24, 8)) // 4/4
    track.add(timeSignature(7680, 6, 3, 24, 8)) // 6/8 after 16 beats
    track.add(timeSignature(7680 + 3 * 480, 3, 2, 24, 8)) // 3/4 after 3 more beats

    MidiTimeSignature.read(seq) shouldBe List(change(0, 4, 4), change(16, 6, 8), change(19, 3, 4))
  }

  it should "come from every track, ordered by time, with events at the same tick in the order of the file" in {
    val seq = sequence()
    val first = seq.createTrack()
    val second = seq.createTrack()
    first.add(timeSignature(4800, 3, 2))
    first.add(timeSignature(0, 2, 1))
    second.add(timeSignature(2400, 5, 2))
    second.add(timeSignature(2400, 7, 3))

    MidiTimeSignature.read(seq) shouldBe
      List(change(0, 2, 2), change(5, 5, 4), change(5, 7, 8), change(10, 3, 4))
  }

  it should "be empty for a sequence without time signature events" in {
    val seq = sequence()
    seq.createTrack()
    MidiTimeSignature.read(seq) shouldBe Nil
  }

  it should "skip malformed events" in {
    val seq = sequence()
    val track = seq.createTrack()
    track.add(timeSignature(0, 4)) // no denominator
    track.add(timeSignature(480, 0, 2, 24, 8)) // no beats in a bar
    track.add(timeSignature(960, 4, 7, 24, 8)) // a 1/128 denominator
    track.add(timeSignature(1440, 3, 2, 24, 8))
    MidiTimeSignature.read(seq) shouldBe List(change(3, 3, 4))
  }

  it should "be empty when the time is not in ticks per quarter note" in {
    val seq = sequence(Sequence.SMPTE_25, 40)
    seq.createTrack().add(timeSignature(0, 3, 2, 24, 8))
    MidiTimeSignature.read(seq) shouldBe Nil
  }

  "The time signatures of a file" should "be those of its events, and empty (4/4 then) when it cannot be read" in {
    val file = MidiTestFiles.write(
      Files.createTempFile("time-signature-test", ".mid"),
      Seq(MidiTestFiles.NoteTrack(0, Seq(60, 62, 64))),
      timeSignatures = Seq((0, 3, 4), (6, 4, 4)))
    try MidiTimeSignature.read(file.toString) shouldBe List(change(0, 3, 4), change(6, 4, 4))
    finally file.toFile.delete()

    val garbage = Files.createTempFile("time-signature-garbage", ".mid")
    try {
      Files.write(garbage, "not a midi file".getBytes)
      MidiTimeSignature.read(garbage.toString) shouldBe Nil
    } finally garbage.toFile.delete()
    MidiTimeSignature.read(new File("does-not-exist.mid").getPath) shouldBe Nil
  }
}
