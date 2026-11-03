package actors.directors

import actors.monitors.{HealthMonitor, HealthMonitorFactory, SimpleHealthMonitor}
import org.apache.pekko.actor._
import designPatterns.observer.{EventNotification, Observer}
import messages._
import messages.consensus.DecisionType
import utils.ActorUtils
import utils.builders.{AtLeastOnce, Count, IsAtLeastOnce, Zero}

import scala.collection.mutable

case class WaitingDirectorBuilder[ActorSysCount <: Count](
    var actorSystem: Option[ActorSystem] = None,
    var healthMonitorFactory: Option[HealthMonitorFactory[Zero, Zero]] = None)
      extends DirectorBuilder[ActorSysCount] {

  override def withActorSystem(actorSystem: ActorSystem) =
    copy[AtLeastOnce](actorSystem = Some(actorSystem))

  def withHealthMonitor(healthMonitorFactory: HealthMonitorFactory[Zero, Zero]) =
    copy[ActorSysCount](healthMonitorFactory = Some(healthMonitorFactory))

  override def build[A <: ActorSysCount : IsAtLeastOnce]: Director =
    new WaitingDirector(this.asInstanceOf[WaitingDirectorBuilder[AtLeastOnce]])
}

object WaitingDirector {
  val DEFAULT_TIMEOUT_MS: Long = 8000
  def builder = new WaitingDirectorBuilder[Zero]()
}

class WaitingDirector(builder: WaitingDirectorBuilder[AtLeastOnce]) extends Director with Observer with ActorLogging {
  implicit val actorSystem: ActorSystem = builder.actorSystem.get
  private var timeTick: Long = 0
  private val playersStillPlaying = new java.util.concurrent.ConcurrentHashMap[ActorPath, Unit]()
  private val healthMonitor: HealthMonitor = {
    builder.healthMonitorFactory
      .getOrElse(SimpleHealthMonitor.builder)
      .withTimeoutMS(WaitingDirector.DEFAULT_TIMEOUT_MS)
      .withActorSystem(actorSystem)
      .build
  }

  healthMonitor.addObserver(this)

  override def start(): Unit = {
    sync()
  }

  override def stop(): Unit = {
    healthMonitor.reset()
    context.become(super.receive)
  }

  override def receive: Receive = {
    super.receive orElse getWaitForChildrenBehaviour
  }

  def getWaitForChildrenBehaviour: Receive = {
    {
      case m: MusicInfoMessage =>
        playersStillPlaying.put(m.sender.path, ())
        healthMonitor.receivedHeartbeat(m.sender)
        if (m.director.isEmpty) {
          log.debug(s"Sending director identity to ${m.sender.path.name}")
          m.sender ! DirectorIdentityInfoMessage(self, self)
        }
      case m: messages.FinishedPlaying =>
        log.debug(s"Player ${m.sender.path.name} finished playing")
        playersStillPlaying.remove(m.sender.path)
        healthMonitor.receivedHeartbeat(m.sender)
        sync()
      case m =>
        log.debug(s"Director Received: ${m.getClass}")
    }
  }

  override def notify(eventNotification: EventNotification): Unit = eventNotification match {
    case SimpleHealthMonitor.IsSuspected(actor) =>
      log.debug(s"${actor.path.name} is suspected to have crashed")
      playersStillPlaying.remove(actor.path)
      sync()
    case _ =>
      log.debug("Unknown event received from HealthMonitor")
  }

  def sync(): Unit = {
    log.debug(s"Waiting on ${playersStillPlaying.size} musician")
    if (playersStillPlaying.isEmpty) {
      log.debug(s"Sending Sync($timeTick)")
      ActorUtils.broadcast(SyncMessage(self, timeTick))
      timeTick += 1
    }
    context.become(receive)
  }

  override protected def haveAllActorsVoted(decisionType: DecisionType): Boolean = {
    log.debug(s"${votesByDecisionType.get(decisionType).map(_.size).getOrElse(0)} actors out of "+
      s"${healthMonitor.getHealthyActors.size} have voted")
    votesByDecisionType.get(decisionType).map(_.size).getOrElse(0) >= healthMonitor.getHealthyActors.size
  }
}
