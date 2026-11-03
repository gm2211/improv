package messages

import org.apache.pekko.actor.ActorRef
import instruments.InstrumentType.InstrumentType
import representation.Phrase

case class MusicInfoMessage(
  phrase: Phrase,
  time: Long,
  instrument: InstrumentType,
  sender: ActorRef,
  director: Option[ActorRef]) extends Message {
}
