package messages.consensus

import messages.consensus.BinaryDecision.{BinaryDecisionAnswer, No, Yes}
import representation.MusicGenre

trait DecisionType {
  def decide(answers: Iterable[DecisionAnswer]): DecisionAnswer
}

trait DecisionAnswer
object InvalidDecision extends DecisionAnswer

object BinaryDecision {
  sealed trait BinaryDecisionAnswer extends DecisionAnswer
  case object Yes extends BinaryDecisionAnswer
  case object No extends BinaryDecisionAnswer
}

trait BinaryDecision extends DecisionType

object UnanimousDecision extends PartialFunction[Iterable[DecisionAnswer], DecisionAnswer] {
  def apply(answers: Iterable[DecisionAnswer]): DecisionAnswer = {
      require(isDefinedAt(answers), "Unanimous decisions are currently only supported for binary voting")
      val unanimousYes = answers.forall { case Yes => true; case _ => false }
      if (unanimousYes) Yes else No
  }

  override def isDefinedAt(answers: Iterable[DecisionAnswer]): Boolean =
    answers.headOption.exists(_.isInstanceOf[BinaryDecisionAnswer])
}

object MajorityDecision extends PartialFunction[Iterable[DecisionAnswer], DecisionAnswer] {
  override def isDefinedAt(x: Iterable[DecisionAnswer]): Boolean = true

  override def apply(answers: Iterable[DecisionAnswer]): DecisionAnswer = {
      // Group equal decisions together; count number of equal decisions; choose majority decision
      answers.groupBy(identity)
        .view.mapValues(_.size).toMap
        .maxBy(_._2)._1
  }
}

case object Termination extends BinaryDecision {
  override def decide(answers: Iterable[DecisionAnswer]): DecisionAnswer =
    UnanimousDecision(answers)
}

case object ReadyToPlay extends BinaryDecision {
  override def decide(answers: Iterable[DecisionAnswer]): DecisionAnswer =
    UnanimousDecision(answers)
}

case object GenreElection extends DecisionType {
  override def decide(answers: Iterable[DecisionAnswer]): DecisionAnswer =
    MajorityDecision(answers)
}

case class GenrePreference(genre: MusicGenre) extends DecisionAnswer