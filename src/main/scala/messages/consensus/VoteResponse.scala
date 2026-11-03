package messages.consensus

import org.apache.pekko.actor.ActorRef

case class VoteResponse(
  override val sender: ActorRef,
  decisionType: DecisionType,
  decision: DecisionAnswer) extends ConsensusMessage
