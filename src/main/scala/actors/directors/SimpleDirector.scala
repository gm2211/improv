package actors.directors

import org.apache.pekko.actor.{ActorLogging, ActorSystem, Cancellable}
import messages.SyncMessage
import messages.consensus.DecisionType
import utils.ActorUtils
import utils.ImplicitConversions.anyToRunnable
import utils.builders.{AtLeastOnce, Count, IsAtLeastOnce, Zero}

case class SimpleDirectorBuilder[ActorSysCount <: Count](
  var actorSystem: Option[ActorSystem] = None,
  var syncFrequencyMS: Option[Long] = None) extends DirectorBuilder[ActorSysCount] {

  override def withActorSystem(actorSystem: ActorSystem) =
    copy[AtLeastOnce](actorSystem = Some(actorSystem))

  def withSyncFrequencyMS(syncFrequencyMS: Long) =
    copy[ActorSysCount](syncFrequencyMS = Some(syncFrequencyMS))

  override def build[A <: ActorSysCount : IsAtLeastOnce]: Director =
    new SimpleDirector(this.asInstanceOf[SimpleDirectorBuilder[AtLeastOnce]])
}

object SimpleDirector {
  def builder = new SimpleDirectorBuilder[Zero]()

  val DEFAULT_SYNC_FREQ_MS: Long = 200
}

class SimpleDirector(builder: SimpleDirectorBuilder[AtLeastOnce]) extends Director with ActorLogging {
  implicit val actorSystem: ActorSystem = builder.actorSystem.get
  val syncFrequencyMS: Long = builder.syncFrequencyMS.getOrElse(SimpleDirector.DEFAULT_SYNC_FREQ_MS)

  private var task: Option[Cancellable] = None
  private var tickCount: Long = 0


  override def start(): Unit = {
    task = Some(
      ActorUtils.schedule(
        delayMS = 0,
        intervalMS = syncFrequencyMS,
        task = () => sync()))
  }

  override def stop(): Unit = {
    task.foreach(_.cancel())
  }

  def sync(): Unit = {
    ActorUtils.broadcast(SyncMessage(self, tickCount))
    tickCount += 1
  }

  override protected def haveAllActorsVoted(decisionType: DecisionType): Boolean = {
    true
  }
}

