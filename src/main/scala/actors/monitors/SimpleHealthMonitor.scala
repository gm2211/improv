package actors.monitors

import org.apache.pekko.actor._
import designPatterns.observer.EventNotification
import messages.MusicInfoMessage
import utils.ActorUtils
import utils.ImplicitConversions.anyToRunnable
import utils.builders._

import scala.collection.mutable

case class SimpleHealthMonitorBuilder[
  TimeoutCount <: Count,
  ActorSystemCount <: Count](
    timeoutMS: Option[Long] = None,
    actorSystem: Option[ActorSystem] = None,
    statsMonitor: Option[StatsMonitor] = None) extends HealthMonitorFactory[TimeoutCount, ActorSystemCount] {
  def withTimeoutMS(timeoutMS: Long) =
    copy[AtLeastOnce, ActorSystemCount](timeoutMS = Some(timeoutMS))

  def withActorSystem(system: ActorSystem) =
    copy[TimeoutCount, AtLeastOnce](actorSystem = Some(system))

  def withStatsMonitor(statsMonitor: StatsMonitor) =
    copy[TimeoutCount, ActorSystemCount](statsMonitor = Some(statsMonitor))

  def build[A <: TimeoutCount : IsAtLeastOnce,
            B <: ActorSystemCount : IsAtLeastOnce]: SimpleHealthMonitor =
    new SimpleHealthMonitor(this.asInstanceOf[SimpleHealthMonitorBuilder[AtLeastOnce, AtLeastOnce]])

  override def buildAsActor[A <: TimeoutCount : IsAtLeastOnce,
                            B <: ActorSystemCount : IsZero](implicit system: ActorSystem): ActorRef = {
    val builder = this.asInstanceOf[SimpleHealthMonitorBuilder[AtLeastOnce, AtLeastOnce]]
    val props = Props(new SimpleHealthMonitor(builder) with Actor {
        override def receive: Actor.Receive = {
          case m: MusicInfoMessage =>
            receivedHeartbeat(m.sender)
        }
      })
    system.actorOf(props)
  }
}

object SimpleHealthMonitor {
  def builder = new SimpleHealthMonitorBuilder[Zero, Zero]

  case class IsSuspected(actor: ActorRef) extends EventNotification

  val DEFAULT_CACHE_SIZE = 1000
  val DEFAULT_TIMEOUT_MS: Long = 5000
  val DEFAULT_HEALTHY_RATIO = 2.0 / 3.0
}

class SimpleHealthMonitor(builder: SimpleHealthMonitorBuilder[AtLeastOnce, AtLeastOnce]) extends HealthMonitor {
  private implicit val actorSystem = builder.actorSystem.get
  val timeoutMS = builder.timeoutMS.getOrElse(SimpleHealthMonitor.DEFAULT_TIMEOUT_MS)
  private val statsMonitor: StatsMonitor = builder.statsMonitor.getOrElse(new SimpleStatsMonitor)
  private val heartbeatsCache = mutable.HashMap[ActorPath, Cancellable]()

  override def isSystemHealthy: Boolean = {
    heartbeatsCache.size >= SimpleHealthMonitor.DEFAULT_HEALTHY_RATIO * statsMonitor.activeActorsCount
  }

  override def receivedHeartbeat(actor: ActorRef): Unit = {
    heartbeatsCache.get(actor.path).foreach(_.cancel())
    val timeoutTask = ActorUtils.scheduleOnce(
      timeoutMS,
      () => notifyObservers(SimpleHealthMonitor.IsSuspected(actor)))

    heartbeatsCache.put(actor.path, timeoutTask)
  }

  override def isActorHealthy(actor: ActorRef): Boolean = heartbeatsCache.contains(actor.path)

  override def getHealthyActors: List[ActorPath] = heartbeatsCache.keySet.toList

  override def reset(): Unit = {
    heartbeatsCache.foreach(_._2.cancel())
    heartbeatsCache.clear()
    statsMonitor.reset()
  }
}
