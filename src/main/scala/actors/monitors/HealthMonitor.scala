package actors.monitors

import org.apache.pekko.actor.{ActorPath, ActorRef, ActorSystem}
import designPatterns.observer.Observable
import utils.builders._

// Type-safe Builders are not feasible through inheritance: As soon as I have more than one implementation
// I'll have to give up either
trait HealthMonitorFactory[A <: Count, B <: Count] {
  def withTimeoutMS(timeout: Long): HealthMonitorFactory[AtLeastOnce, B]
  def withActorSystem(actorSystem: ActorSystem): HealthMonitorFactory[A, AtLeastOnce]
  def build[C <: A : IsAtLeastOnce, D <: B : IsAtLeastOnce ]: HealthMonitor
  def buildAsActor[C <: A : IsAtLeastOnce, D <: B : IsZero](implicit system: ActorSystem): ActorRef
}

trait HealthMonitor extends Monitor with Observable {
  def receivedHeartbeat(actor: ActorRef)
  def isSystemHealthy: Boolean
  def isActorHealthy(actor: ActorRef): Boolean
  def getHealthyActors: List[ActorPath]
  def reset(): Unit
}
