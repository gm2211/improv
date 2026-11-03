package actors.musicians.behaviour

import actors.musicians.behaviour.BoredomBehaviour.Bored
import designPatterns.observer.{EventNotification, Observable}
import messages.consensus.BinaryDecision.BinaryDecisionAnswer
import messages.consensus._
import messages.{Message, SyncMessage}

import scala.util.Random

object BoredomBehaviour {
  def getRandBoredomThreshold = Random.nextInt(30)

  def getRandExtremeBoredomThreshold = 31 + Random.nextInt(10)

  case object Bored extends EventNotification

}

class BoredomBehaviour(
    val boredomThreshold: Double = BoredomBehaviour.getRandBoredomThreshold,
    val extremeBoredomThreshold: Double = BoredomBehaviour.getRandExtremeBoredomThreshold,
    private val boredomFunc: Double => Double = (b: Double) => scala.math.pow(2, b / 1000))
      extends AIMusicianBehaviour with ReceiveBehaviour with Observable {
  var boredom = 0.0
  var votingInProgress = false
  var votedAlready = false

  override def apply(message: Message): Unit = message match {
    case sync: SyncMessage =>
      boredom = boredomFunc(sync.time.toDouble)

    case VoteRequest(sender, decisionType) =>
      val fromDirector = musician.exists(_.directorIdentity.exists(_.path == sender.path))
      decisionType match {
        case Termination if ! votedAlready && fromDirector =>
          votingInProgress = true
          votedAlready = true
          var vote: BinaryDecisionAnswer = BinaryDecision.No
          if (boredom > boredomThreshold) {
            vote = BinaryDecision.Yes
          }
          musician.foreach(m => sender ! VoteResponse(m.self, Termination, vote))
        case _ =>
      }
    case f: FinalDecision =>
      votingInProgress = false
      votedAlready = false

    case c: ConsensusMessage =>
      () // Ignore other consensus messages

    case _ =>
      if (boredom > extremeBoredomThreshold && ! votingInProgress) {
        musician.foreach{ mus =>
          mus.notify(Bored)
          votingInProgress = true
        }
      }
  }

}
