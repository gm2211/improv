package messages

import org.apache.pekko.actor.ActorRef

case class Start(override val sender: ActorRef) extends Message
