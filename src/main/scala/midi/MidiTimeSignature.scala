package midi

import org.slf4j.LoggerFactory
import representation.{TimeSignature, TimeSignatureChange}

import java.io.File
import javax.sound.midi.{MetaMessage, MidiSystem, Sequence}
import scala.util.control.NonFatal

/**
 * The time signatures of a MIDI file, read from its time signature meta events (type 0x58) with `javax.sound.midi`.
 *
 * Not through jMusic: its reader mis-parses a SysEx message that follows a known event and can swallow the events after
 * it (#31), the same reason the tempo is read by [[MidiTempo]]. The bar lines of a piece come from these events (#27).
 */
object MidiTimeSignature {
  private val log = LoggerFactory.getLogger(getClass)

  private val TIME_SIGNATURE_META_TYPE = 0x58

  /** A denominator of 2^6 = 64 is the smallest note value MIDI files use in practice; anything beyond is malformed. */
  private val MAX_DENOMINATOR_POWER = 6

  /** The time signature events of the file, in order; empty when it has none or cannot be read (see the sequence form). */
  def read(filename: String): List[TimeSignatureChange] =
    try read(MidiSystem.getSequence(new File(filename)))
    catch {
      case NonFatal(error) =>
        log.warn(s"Could not read the time signatures of $filename (${error.getClass.getSimpleName}), assuming 4/4")
        Nil
    }

  /**
   * The time signature events of every track, ordered by time (events on the same tick keep the order of the tracks
   * and of the file, so the last of them is the one that holds), with the start in quarter notes. An event that is
   * malformed (no numerator, or a denominator over 64) is skipped. Empty for a sequence without events or one whose time
   * is not in ticks per quarter note (SMPTE), where beats cannot be derived.
   */
  def read(sequence: Sequence): List[TimeSignatureChange] = {
    val resolution = sequence.getResolution
    if (sequence.getDivisionType != Sequence.PPQ || resolution <= 0) return Nil

    sequence.getTracks.toList.flatMap { track =>
      (0 until track.size).flatMap { index =>
        val event = track.get(index)
        event.getMessage match {
          case meta: MetaMessage if meta.getType == TIME_SIGNATURE_META_TYPE =>
            parse(meta.getData).map(signature => (event.getTick, signature))
          case _ => None
        }
      }
    }.sortBy(_._1).map { case (tick, signature) =>
      TimeSignatureChange(BigDecimal(tick) / resolution, signature)
    }
  }

  /** The data of a time signature event: numerator, denominator as a power of two, then two fields that do not matter here. */
  private def parse(data: Array[Byte]): Option[TimeSignature] = {
    if (data.length < 2) return None
    val numerator = data(0) & 0xff
    val denominatorPower = data(1) & 0xff
    Option.when(numerator > 0 && denominatorPower <= MAX_DENOMINATOR_POWER)(TimeSignature(numerator, 1 << denominatorPower))
  }
}
