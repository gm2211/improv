package messages

import org.apache.pekko.actor.ActorRef

trait Message {
  implicit val sender: ActorRef
}
