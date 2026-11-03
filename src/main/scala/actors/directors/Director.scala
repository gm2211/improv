package actors.directors

import utils.collections.addBinding
import org.apache.pekko.actor.{ActorRef, ActorLogging, Actor, ActorSystem}
import messages.consensus.BinaryDecision.{No, Yes}
import messages.consensus._
import messages.{Start, Stop}
import representation.MusicGenre
import utils.ActorUtils
import utils.builders.{AtLeastOnce, Count, IsAtLeastOnce}
import utils.collections.CollectionUtils

import scala.collection.mutable

trait Director extends Actor with ActorLogging {
  implicit val actorSystem: ActorSystem
  val votesByDecisionType = CollectionUtils.createHashMultimap[DecisionType, VoteResponse]
  val votingsInProgress = mutable.HashSet[DecisionType]()
  var musicGenre: Option[MusicGenre] = None

  def start(): Unit
  def stop(): Unit

  protected def haveAllActorsVoted(decisionType: DecisionType): Boolean


  override def receive: Receive = {
    case Start =>
      start()

    case Stop =>
      stop()

    case VoteRequest(sender, decisionType) =>
      if (sender.path != self.path) {
        log.debug(s"Received request for $decisionType")
        processVotingRequest(sender, decisionType)
      }

    case voteResponse: VoteResponse if votingsInProgress.contains(voteResponse.decisionType) =>
      log.debug(s"Received response $voteResponse")
      votesByDecisionType.addBinding(voteResponse.decisionType, voteResponse)

      if (haveAllActorsVoted(voteResponse.decisionType)) {
        val votes = votesByDecisionType.get(voteResponse.decisionType).get.map(_.decision)
        val decision = voteResponse.decisionType.decide(votes)

        votesByDecisionType.remove(voteResponse.decisionType)
        votingsInProgress.remove(voteResponse.decisionType)
        processDecision(voteResponse.decisionType, decision)
      }
  }

  def processVotingRequest(sender: ActorRef, decisionType: DecisionType) = {
    decisionType match {
      case GenreElection if musicGenre.isDefined =>
        sender ! FinalDecision(self, GenrePreference(musicGenre.get))
      case _ =>
        startVoting(decisionType)
    }
  }

  private def startVoting(decisionType: DecisionType): Unit = {
    if (!votingsInProgress.contains(decisionType)) {
      log.debug(s"Starting voting process for $decisionType")
      votingsInProgress.add(decisionType)
      ActorUtils.broadcast(VoteRequest(self, decisionType))
    }
  }

  private def processDecision(decisionType: DecisionType, decision: DecisionAnswer) = decisionType match {
    case Termination =>
      decision match {
        case Yes =>
          ActorUtils.broadcast(FinalDecision(self, decision))
          stop()
          actorSystem.terminate()
        case _ =>
      }

    case GenreElection =>
      musicGenre = Some(decision.asInstanceOf[GenrePreference].genre)
      log.debug(s"Broadcasting $decision")
      ActorUtils.broadcast(FinalDecision(self, decision))

      log.debug("Ask musicians if they are ready")
      startVoting(ReadyToPlay)

    case ReadyToPlay =>
      decision match {
        case Yes =>
          log.debug("Everybody is ready")
          start()
        case No =>
          log.debug("Somebody is not ready")
          startVoting(ReadyToPlay)
      }

    case _ =>
  }
}

trait DirectorBuilder[A <: Count] {
  def build[T <: A : IsAtLeastOnce]: Director
  def withActorSystem(actorSystem: ActorSystem): DirectorBuilder[AtLeastOnce]
}
