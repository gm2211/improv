package messages.consensus

import org.apache.pekko.actor.ActorRef

case class FinalDecision(override val sender: ActorRef, decision: DecisionAnswer) extends ConsensusMessage
