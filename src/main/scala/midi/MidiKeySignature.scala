package midi

import org.slf4j.LoggerFactory
import representation.{KeySignature, KeySignatureChange}

import java.io.File
import javax.sound.midi.{MetaMessage, MidiSystem, Sequence}
import scala.util.control.NonFatal

/**
 * The key signatures of a MIDI file, read from its key signature meta events (type 0x59) with `javax.sound.midi`.
 *
 * jMusic keeps one key per score, the last event it reads, so a piece that changes key gets its last key throughout
 * (#41); and its reader mis-parses a SysEx message that follows a known event (#31), the reason the tempo and the time
 * signatures are read this way too ([[MidiTempo]], [[MidiTimeSignature]]).
 */
object MidiKeySignature {
  private val log = LoggerFactory.getLogger(getClass)

  private val KEY_SIGNATURE_META_TYPE = 0x59

  /** The key signature events of the file, in order; empty when it has none or cannot be read (see the sequence form). */
  def read(filename: String): List[KeySignatureChange] =
    try read(MidiSystem.getSequence(new File(filename)))
    catch {
      case NonFatal(error) =>
        log.warn(s"Could not read the key signatures of $filename (${error.getClass.getSimpleName}), keeping jMusic's key")
        Nil
    }

  /**
   * The key signature events of every track, ordered by time (events on the same tick keep the order of the tracks and
   * of the file, so the last of them is the one that holds), with the start in quarter notes. A malformed event (fewer
   * than 2 bytes, more than 7 sharps or flats, a mode other than 0 = major or 1 = minor) is skipped. Empty for a sequence
   * without events or one whose time is not in ticks per quarter note (SMPTE).
   */
  def read(sequence: Sequence): List[KeySignatureChange] = {
    val resolution = sequence.getResolution
    if (sequence.getDivisionType != Sequence.PPQ || resolution <= 0) return Nil

    sequence.getTracks.toList.flatMap { track =>
      (0 until track.size).flatMap { index =>
        val event = track.get(index)
        event.getMessage match {
          case meta: MetaMessage if meta.getType == KEY_SIGNATURE_META_TYPE =>
            parse(meta.getData).map(key => (event.getTick, key))
          case _ => None
        }
      }
    }.sortBy(_._1).map { case (tick, key) => KeySignatureChange(BigDecimal(tick) / resolution, key) }
  }

  /**
   * The key that lasts longest in the file (see [[KeySignatureChange.longestIn]], from the start to the end of the
   * sequence), or `None` when the file cannot be read or has no key signature event (the caller keeps jMusic's key).
   */
  def longestLasting(filename: String): Option[KeySignature] =
    try longestLasting(MidiSystem.getSequence(new File(filename)))
    catch {
      case NonFatal(error) =>
        log.warn(s"Could not read the key signatures of $filename (${error.getClass.getSimpleName}), keeping jMusic's key")
        None
    }

  def longestLasting(sequence: Sequence): Option[KeySignature] = {
    val changes = read(sequence)
    Option.when(changes.nonEmpty) {
      val end = (BigDecimal(sequence.getTickLength) / sequence.getResolution).max(changes.map(_.startBeat).max)
      KeySignatureChange.longestIn(changes, BigDecimal(0), end)
    }
  }

  /** The data of a key signature event: the number of sharps as a signed byte (negative: flats), then 0 major / 1 minor. */
  private def parse(data: Array[Byte]): Option[KeySignature] = {
    if (data.length < 2) return None
    val sharps = data(0).toInt // signed
    val mode = data(1) & 0xff
    Option.when(sharps >= -7 && sharps <= 7 && (mode == 0 || mode == 1))(KeySignatureChange.fromMidi(sharps, mode == 1))
  }
}
