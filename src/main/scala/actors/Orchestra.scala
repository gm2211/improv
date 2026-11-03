package actors

import actors.directors.{DirectorBuilder, SimpleDirector}
import actors.musicians.Musician
import org.apache.pekko.actor.{ActorRef, ActorSystem, Props}
import messages.{Message, Start, Stop}
import utils.ActorUtils
import utils.ImplicitConversions.anyToRunnable
import utils.builders.Count

import scala.language.implicitConversions

case class OrchestraBuilder(
  var name: Option[String] = None,
  var actorSystem: Option[ActorSystem] = None,
  var director: Option[DirectorBuilder[_ <: Count]] = None) {
  def withName(name: Option[String]) = copy(name = name)

  def withActorSystem(actorSystem: ActorSystem) = copy(actorSystem = Some(actorSystem))

  def withDirector(director: DirectorBuilder[_ <: Count]) = copy(director = Some(director))

  def build: Orchestra = new Orchestra(this)
}

object Orchestra {
  def builder: OrchestraBuilder = new OrchestraBuilder()
}

class Orchestra(val builder: OrchestraBuilder) {
  val DEFAULT_NAME: String = "orchestra"
  val name: String = builder.name.getOrElse(DEFAULT_NAME)
  implicit val system: ActorSystem = builder.actorSystem.getOrElse(ActorSystem.create(name))
  private val director: ActorRef = {
    system.actorOf(Props(builder.director.getOrElse(SimpleDirector.builder).withActorSystem(system).build), "director")
  }
  private var musicianCount = 0
  ActorUtils.subscribe(director, classOf[Message])


  def registerMusician(musician: ActorSystem => Musician): Unit = {
    val actor = system.actorOf(Props(musician(system)), s"musician$musicianCount")
    ActorUtils.subscribe(actor, classOf[Message])
    musicianCount += 1
  }

  def start(): Unit = {
    director ! Start
  }

  def shutdown(): Unit = {
    system.log.debug("Shutting down..")
    system.terminate()
  }

  def pause(): Unit = director ! Stop

  def shutdown(delay: Long): Unit = {
    ActorUtils.scheduleOnce(delay, () => shutdown())
  }
}

