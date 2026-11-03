package midi

import midi.MidiTestFiles.sequenceWithTempos
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import javax.sound.midi.{MetaMessage, MidiEvent, Sequence}

class MidiTempoTest extends AnyFlatSpec with Matchers {
  private def longest(lengthBeats: Int, tempos: (Int, Double)*): Option[Double] =
    MidiTempo.longestLastingBPM(sequenceWithTempos(lengthBeats, tempos*))

  "The longest lasting tempo" should "be None for a sequence without tempo events" in {
    longest(100) shouldBe None
  }

  it should "be the tempo of a file with a single tempo" in {
    longest(200, 0 -> 100.0).value shouldBe 100.0 +- 1e-4
    longest(200, 50 -> 150.0).value shouldBe 150.0 +- 1e-4 // 60 s, against 25 s of the 120 BPM default before it
  }

  it should "not be the last tempo, which may be a short ritardando" in {
    // 120 BPM for 100 beats, then slowing down over the last 12 beats
    longest(112, 0 -> 120.0, 100 -> 90.0, 108 -> 60.0).value shouldBe 120.0 +- 1e-4
  }

  it should "not be the first tempo either" in {
    // 60 BPM for the first 6 beats, 120 BPM for 84, 100 BPM for 20, then a tempo event at the very end
    longest(110, 0 -> 60.0, 6 -> 120.0, 90 -> 100.0, 110 -> 16.0).value shouldBe 120.0 +- 1e-4
  }

  it should "measure how long a tempo lasts in time, not in beats" in {
    // 300 beats at 240 BPM are 75 s, 100 beats at 60 BPM are 100 s
    longest(400, 0 -> 240.0, 300 -> 60.0).value shouldBe 60.0 +- 1e-4
  }

  it should "add up the stretches that have the same tempo" in {
    // 100 BPM for 40 beats (24 s) twice, 200 BPM for 100 beats (30 s) in between: 48 s against 30 s
    longest(180, 0 -> 100.0, 40 -> 200.0, 140 -> 100.0).value shouldBe 100.0 +- 1e-4
  }

  it should "count the MIDI default of 120 BPM before the first tempo event" in {
    // 40 beats at the default (20 s), then 10 beats at 60 BPM (10 s)
    longest(50, 40 -> 60.0).value shouldBe 120.0 +- 1e-4
  }

  it should "go to the first of two tempos that last equally long" in {
    // 10 beats at 120 BPM and 5 beats at 60 BPM are both 5 s
    longest(15, 0 -> 120.0, 10 -> 60.0).value shouldBe 120.0 +- 1e-4
    longest(15, 0 -> 60.0, 5 -> 120.0).value shouldBe 60.0 +- 1e-4
  }

  it should "use the last of the tempo events that sit on the same tick" in {
    longest(10, 0 -> 100.0, 0 -> 80.0).value shouldBe 80.0 +- 1e-4
  }

  it should "read tempo events from every track and ignore malformed ones" in {
    val sequence = new Sequence(Sequence.PPQ, MidiTestFiles.TICKS_PER_QUARTER)
    sequence.createTrack().add(MidiTestFiles.tempoEvent(0, 75.0))
    val second = sequence.createTrack()
    second.add(new MidiEvent(new MetaMessage(0x51, Array[Byte](1, 2), 2), 0)) // too short to be a tempo
    second.add(new MidiEvent(new MetaMessage(0x51, Array[Byte](0, 0, 0), 3), 480)) // 0 microseconds per quarter note
    second.add(MidiTestFiles.tempoEvent(100, 80.0)) // 10 beats until the end: shorter than the 100 at 75 BPM
    second.add(MidiTestFiles.tempoEvent(110, 80.0))
    MidiTempo.longestLastingBPM(sequence).value shouldBe 75.0 +- 1e-4
  }

  it should "be None for a sequence in SMPTE time" in {
    val sequence = new Sequence(Sequence.SMPTE_25, 40)
    sequence.createTrack().add(MidiTestFiles.tempoEvent(0, 100.0))
    MidiTempo.longestLastingBPM(sequence) shouldBe None
  }

  it should "be None for a file that cannot be read" in {
    MidiTempo.longestLastingBPM("/no/such/dir/missing.mid") shouldBe None
  }

  private implicit class OptionValue(option: Option[Double]) {
    def value: Double = option.getOrElse(fail("expected a tempo, got None"))
  }
}
