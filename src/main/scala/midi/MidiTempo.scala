package midi

import org.slf4j.LoggerFactory

import java.io.File
import javax.sound.midi.{MetaMessage, MidiSystem, Sequence}
import scala.collection.mutable
import scala.util.control.NonFatal

/**
 * The tempo of a MIDI file, read from its tempo meta events.
 *
 * jMusic keeps one tempo per score: the last tempo event it reads (a final ritardando, say), and for some files a
 * wrong value (`waterloo.mid`: a SysEx message that follows the tempo event makes jMusic's reader parse the SysEx bytes
 * into the tempo event, 149 BPM becomes 5.17). Every note time in improv is converted from beats to nanoseconds with
 * that one tempo, so a wrong tempo changes the length of every phrase that is cut by seconds (#23).
 */
object MidiTempo {
  private val log = LoggerFactory.getLogger(getClass)

  private val TEMPO_META_TYPE = 0x51

  /** The MIDI default, in force until the first tempo event of a file: 120 BPM. */
  private val DEFAULT_MICROS_PER_QUARTER = 500000L

  /**
   * The tempo that lasts longest in the file, in beats (quarter notes) per minute, or `None` when it cannot be
   * read or the file has no tempo event (the caller keeps jMusic's value then).
   */
  def longestLastingBPM(filename: String): Option[Double] =
    try longestLastingBPM(MidiSystem.getSequence(new File(filename)))
    catch {
      case NonFatal(error) =>
        log.warn(s"Could not read the tempo events of $filename (${error.getClass.getSimpleName}), keeping jMusic's tempo")
        None
    }

  /**
   * The tempo that lasts longest in the sequence, in beats per minute. Lasting is real time: the time between a tempo
   * event and the next one (the last one holds until the end of the sequence), summed over all the stretches that have
   * the same tempo, with the MIDI default of 120 BPM before the first event. The first of the tempos wins a tie.
   * `None` for a sequence without tempo events or one whose time is not in ticks per quarter note (SMPTE).
   */
  def longestLastingBPM(sequence: Sequence): Option[Double] = {
    val resolution = sequence.getResolution
    if (sequence.getDivisionType != Sequence.PPQ || resolution <= 0) return None

    // (tick, microseconds per quarter note) of every tempo event of every track, in file order for equal ticks
    val events = sequence.getTracks.toList.flatMap { track =>
      (0 until track.size).flatMap { index =>
        val event = track.get(index)
        event.getMessage match {
          case meta: MetaMessage if meta.getType == TEMPO_META_TYPE && meta.getData.length == 3 =>
            val data = meta.getData.map(_ & 0xffL)
            val micros = (data(0) << 16) | (data(1) << 8) | data(2)
            Option.when(micros > 0)((event.getTick, micros))
          case _ => None
        }
      }
    }.sortBy(_._1)
    if (events.isEmpty) return None

    val end = Math.max(sequence.getTickLength, events.last._1)
    val stretches = (if (events.head._1 > 0) List((0L, DEFAULT_MICROS_PER_QUARTER)) else Nil) ++ events
    val microsByTempo = mutable.LinkedHashMap[Long, Double]()
    stretches.zip(stretches.drop(1).map(_._1) :+ end).foreach { case ((from, tempo), until) =>
      // ticks -> microseconds at this tempo: ticks / (ticks per quarter) quarters of `tempo` microseconds each
      val micros = (until - from).toDouble / resolution * tempo
      microsByTempo(tempo) = microsByTempo.getOrElse(tempo, 0.0) + micros
    }
    val (longest, _) = microsByTempo.maxBy(_._2)
    Some(toBPM(longest))
  }

  /**
   * Beats per minute of a tempo given in microseconds per quarter note. This is the single precision arithmetic of
   * jMusic's `TempoEvent.read` on purpose: a tempo that jMusic read right stays the very same number (124.999992 for
   * `king-of-the-road.mid`, not 125), because even a difference of 1e-7 BPM moves notes across the edge of a 5 second
   * window and changes the phrases of the song (that file would get 24 more pairs).
   */
  private def toBPM(microsPerQuarter: Long): Double = (1000000f / microsPerQuarter.toFloat * 60f).toDouble
}
