package messages.consensus

import org.apache.pekko.actor.ActorRef

case class VoteRequest(
  override val sender: ActorRef, 
  decisionType: DecisionType) extends ConsensusMessage

