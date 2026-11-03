package messages

import org.apache.pekko.actor.ActorRef

case class Stop(override val sender: ActorRef) extends Message
