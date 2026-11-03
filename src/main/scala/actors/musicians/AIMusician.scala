package actors.musicians

import actors.composers.{Composer, RandomComposer}
import actors.musicians.behaviour._
import org.apache.pekko.actor.{ActorLogging, ActorRef, ActorSystem, Props}
import cbr.MusicalCase
import designPatterns.observer.{EventNotification, Observer}
import instruments.{AsyncInstrument, Instrument}
import messages.consensus.{Termination, VoteRequest}
import messages.{DirectorIdentityInfoMessage, FinishedPlaying, Message, MusicInfoMessage}
import representation.{MusicGenre, Phrase}
import utils.ActorUtils
import utils.ImplicitConversions.toEnhancedIterable
import utils.builders.{AtLeastOnce, Count, IsAtLeastOnce, Zero}
import utils.collections.{BoundedStack, CollectionUtils}
import utils.functional.FunctionalUtils

import scala.language.implicitConversions

case class AIMusicianBuilder[InstrumentCount <: Count,
 ActorSysCount <: Count](
  var instrument: Option[Instrument] = None,
  var behaviours: Option[List[ActorBehaviour]] = Some(AIMusician.getDefaultBehaviours),
  var actorSystem: Option[ActorSystem] = None,
  var composer: Option[Composer] = None,
  var messageOnly: Option[Boolean] = Some(false)) {

  def withInstrument(instrument: Instrument) =
    copy[AtLeastOnce, ActorSysCount](instrument = Some(instrument))
  
  def addBehaviour(behaviour: ActorBehaviour) =
    copy[InstrumentCount, ActorSysCount](behaviours = Some(behaviours.toList.flatten :+ behaviour))

  def withBehaviours(behaviours: List[ActorBehaviour]) =
    copy[InstrumentCount, ActorSysCount](behaviours = Some(behaviours))

  def withActorSystem(actorSystem: ActorSystem) =
    copy[InstrumentCount, AtLeastOnce](actorSystem = Some(actorSystem))

  def withComposer(composer: Composer) =
    copy[InstrumentCount, ActorSysCount](composer = Some(composer))

  def isMessageOnly =
    copy[InstrumentCount, ActorSysCount](messageOnly = Some(true))

  def build[
  A <: InstrumentCount : IsAtLeastOnce,
  C <: ActorSysCount : IsAtLeastOnce]: AIMusician = {
    new AIMusician(this.asInstanceOf[AIMusicianBuilder[AtLeastOnce, AtLeastOnce]])
  }

  def buildProps[
  A <: InstrumentCount : IsAtLeastOnce,
  C <: ActorSysCount : IsAtLeastOnce]: Props = Props(build)
}

object AIMusician {
  def getDefaultBehaviours: List[ActorBehaviour] = {
    List(
      new SyncMessageReceivedBehaviour,
      new MusicMessageInfoReceivedBehaviour,
      new BoredomBehaviour,
      new GenreSelectionBehaviour,
      new ReadyToPlayBehaviour
    )
  }

  def builder: AIMusicianBuilder[Zero, Zero] = new AIMusicianBuilder[Zero, Zero]

  implicit def toFunc(builder: AIMusicianBuilder[AtLeastOnce, Zero]): ActorSystem => AIMusician =
    (actorSystem: ActorSystem) => builder.withActorSystem(actorSystem).build
}

class AIMusician(builder: AIMusicianBuilder[AtLeastOnce, AtLeastOnce])
    extends Musician with ActorLogging with Observer {

  private val instrument: Instrument = builder.instrument.get
  var musicGenre: Option[MusicGenre] = None
  private val behaviours: List[ActorBehaviour] = builder.behaviours.get
  private val receiveBehaviours = behaviours.filterByType[ReceiveBehaviour]
  implicit private val actorSystem: ActorSystem = builder.actorSystem.get
  private val musicComposer: Composer = builder.composer.getOrElse(new RandomComposer)
  private val messageOnly: Boolean = builder.messageOnly.get
  private val playedPhrases = BoundedStack[Phrase](1)
  var directorIdentity: Option[ActorRef] = None

  instrument match {
    case instr: AsyncInstrument =>
      instr.addObserver(this)
    case _ =>
  }

  private[musicians] val musicInfoMessageCache =
    CollectionUtils.createHashMultimap[Long, MusicInfoMessage]
  private[musicians] var currentMusicTime: Long = 0

  behaviours.foreach{ case b: AIMusicianBehaviour => b.registerMusician(this) case _ => }

  def readyToPlay: Boolean = musicGenre.isDefined

  def play(time: Long): Unit = {
    musicGenre.foreach { genre =>
      val instrumentsAndPhrases = musicInfoMessageCache.get(time)
        .map(_.map(m => MusicalCase(m.instrument, genre, m.phrase))).getOrElse(Set())

      musicInfoMessageCache.remove(time)

      val constraints = getCompositionConstraints
      val previouslyPlayedPhrase = playedPhrases.peek.map{ phrase =>
        MusicalCase(instrument.instrumentType, phrase = phrase)
      }

      val responsePhrase = musicComposer.compose(
        previouslyPlayedPhrase,
        instrumentsAndPhrases,
        constraints)

      responsePhrase.foreach{phrase =>
        play(phrase)
        playedPhrases.push(phrase)
      }
    }
  }

  override def play(phrase: Phrase): Unit = {
    if (!messageOnly) instrument.play(phrase)

    log.debug(s"Playing $phrase")
    ActorUtils.broadcast(
      MusicInfoMessage(
        phrase,
        currentMusicTime,
        instrument.instrumentType,
        self,
        directorIdentity))
  }

  override def receive = {
    case DirectorIdentityInfoMessage(_, director_) =>
      log.debug(s"${self.path.name} got message from director")
      this.directorIdentity = Some(director_)
    case m: Message =>
      receiveBehaviours.foreach(_(m))
  }

  override def notify(eventNotification: EventNotification): Unit = eventNotification match {
    case AsyncInstrument.FinishedPlaying =>
      log.debug(s"${self.path.name} sending finished playing to ${directorIdentity.map(_.path.name)}")
      directorIdentity.foreach(_ ! FinishedPlaying(self))
    case BoredomBehaviour.Bored =>
      log.debug(s"${self.path.name} sending bored to ${directorIdentity.map(_.path.name)}")
      directorIdentity.foreach(_ ! VoteRequest(self, Termination))
  }

  private def getCompositionConstraints: List[MusicalCase => Boolean] = {
    List(
      (m) => m.instrumentType.sameInstrumentClass(instrument.instrumentType)
    )
  }
}
