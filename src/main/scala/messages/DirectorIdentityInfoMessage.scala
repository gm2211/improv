package messages

import org.apache.pekko.actor.ActorRef

case class DirectorIdentityInfoMessage(
  override val sender: ActorRef,
  director: ActorRef) extends Message
