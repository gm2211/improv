package midi

import java.io.InputStream
import java.nio.file.{Files, Path}
import javax.sound.midi.{MidiEvent, MidiSystem, Sequence, ShortMessage}
import scala.util.Using

/** Builds the small MIDI files that the parser tests read, and copies resources to a place jMusic can open them from. */
object MidiTestFiles {
  /** A track of quarter notes on one MIDI channel; `program` is a program change at tick 0 (none: the track has none). */
  final case class NoteTrack(channel: Int, pitches: Seq[Int], program: Option[Int] = None)

  val TICKS_PER_QUARTER = 480

  /** Writes a format 1 file with one track per entry. */
  def write(path: Path, tracks: Seq[NoteTrack]): Path = {
    val sequence = new Sequence(Sequence.PPQ, TICKS_PER_QUARTER)
    tracks.foreach { spec =>
      val track = sequence.createTrack()
      spec.program.foreach { program =>
        track.add(new MidiEvent(new ShortMessage(ShortMessage.PROGRAM_CHANGE, spec.channel, program, 0), 0))
      }
      spec.pitches.zipWithIndex.foreach { case (pitch, index) =>
        val start = index.toLong * TICKS_PER_QUARTER
        track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, spec.channel, pitch, 90), start))
        track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, spec.channel, pitch, 0), start + TICKS_PER_QUARTER))
      }
    }
    MidiSystem.write(sequence, 1, path.toFile)
    path
  }

  /** Copies a class path resource (e.g. `trainingMIDIs/cajun-fiddle.mid`) to `target`. */
  def copyResource(resource: String, target: Path): Path = {
    Using.resource(getClass.getClassLoader.getResourceAsStream(resource)) { (in: InputStream) =>
      Files.copy(in, target)
    }
    target
  }
}
