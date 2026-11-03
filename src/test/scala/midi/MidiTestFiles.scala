package midi

import java.io.InputStream
import java.nio.file.{Files, Path, StandardCopyOption}
import javax.sound.midi.{MetaMessage, MidiEvent, MidiSystem, Sequence, ShortMessage, Track}
import scala.util.Using

/** Builds the small MIDI files that the parser tests read, and copies resources to a place jMusic can open them from. */
object MidiTestFiles {
  /** A track of quarter notes on one MIDI channel; `program` is a program change at tick 0 (none: the track has none). */
  final case class NoteTrack(channel: Int, pitches: Seq[Int], program: Option[Int] = None)

  val TICKS_PER_QUARTER = 480

  /** A tempo meta event (type 0x51) at `beat`, `bpm` beats per minute. */
  def tempoEvent(beat: Int, bpm: Double): MidiEvent = {
    val micros = Math.round(60000000.0 / bpm).toInt
    val data = Array((micros >> 16).toByte, (micros >> 8).toByte, micros.toByte)
    new MidiEvent(new MetaMessage(0x51, data, 3), beat.toLong * TICKS_PER_QUARTER)
  }

  /** A time signature meta event (type 0x58) at `beat`: `numerator` over `denominator` (a power of two), 24 clocks per click, 8 32nds per quarter. */
  def timeSignatureEvent(beat: Int, numerator: Int, denominator: Int): MidiEvent = {
    val data = Array(numerator.toByte, Integer.numberOfTrailingZeros(denominator).toByte, 24.toByte, 8.toByte)
    new MidiEvent(new MetaMessage(0x58, data, 4), beat.toLong * TICKS_PER_QUARTER)
  }

  /** A sequence with a single track that holds the given tempo events (beat, bpm) and lasts `lengthBeats` beats. */
  def sequenceWithTempos(lengthBeats: Int, tempos: (Int, Double)*): Sequence = {
    val sequence = new Sequence(Sequence.PPQ, TICKS_PER_QUARTER)
    val track = sequence.createTrack()
    tempos.foreach { case (beat, bpm) => track.add(tempoEvent(beat, bpm)) }
    addNote(track, 0, 60, lengthBeats - 1)
    sequence
  }

  /**
   * Writes a format 1 file with a tempo track (when `tempos` or `timeSignatures` is not empty) and one track per entry of
   * `tracks`. `timeSignatures` are (beat, numerator, denominator).
   */
  def write(
      path: Path,
      tracks: Seq[NoteTrack],
      tempos: Seq[(Int, Double)] = Nil,
      timeSignatures: Seq[(Int, Int, Int)] = Nil): Path = {
    val sequence = new Sequence(Sequence.PPQ, TICKS_PER_QUARTER)
    if (tempos.nonEmpty || timeSignatures.nonEmpty) {
      val tempoTrack = sequence.createTrack()
      tempos.foreach { case (beat, bpm) => tempoTrack.add(tempoEvent(beat, bpm)) }
      timeSignatures.foreach { case (beat, numerator, denominator) =>
        tempoTrack.add(timeSignatureEvent(beat, numerator, denominator))
      }
    }
    tracks.foreach { spec =>
      val track = sequence.createTrack()
      spec.program.foreach { program =>
        track.add(new MidiEvent(new ShortMessage(ShortMessage.PROGRAM_CHANGE, spec.channel, program, 0), 0))
      }
      spec.pitches.zipWithIndex.foreach { case (pitch, index) => addNote(track, spec.channel, pitch, index) }
    }
    MidiSystem.write(sequence, 1, path.toFile)
    path
  }

  /** A quarter note on `beat` (0 based). */
  private def addNote(track: Track, channel: Int, pitch: Int, beat: Int): Unit = {
    val start = beat.toLong * TICKS_PER_QUARTER
    track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, channel, pitch, 90), start))
    track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, channel, pitch, 0), start + TICKS_PER_QUARTER))
  }

  /** Copies a class path resource (e.g. `trainingMIDIs/cajun-fiddle.mid`) to `target`. */
  def copyResource(resource: String, target: Path): Path = {
    Using.resource(getClass.getClassLoader.getResourceAsStream(resource)) { (in: InputStream) =>
      Files.copy(in, target, StandardCopyOption.REPLACE_EXISTING)
    }
    target
  }
}
