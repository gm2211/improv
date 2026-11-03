package messages

import org.apache.pekko.actor.ActorRef

case class FinishedPlaying(sender: ActorRef) extends Message
