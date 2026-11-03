package messages

import org.apache.pekko.actor.ActorRef

case class SyncMessage(override val sender: ActorRef, time: Long) extends Message {
}
