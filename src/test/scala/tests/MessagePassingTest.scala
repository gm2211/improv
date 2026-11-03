package tests

import _root_.instruments.Instrument
import _root_.utils.ImplicitConversions._
import actors.musicians.AIMusicianBuilder
import actors.musicians.behaviour.{MusicMessageInfoReceivedBehaviour, SyncMessageReceivedBehaviour}
import org.apache.pekko.actor.{ActorSystem, Props}
import org.apache.pekko.testkit.TestActorRef
import messages.SyncMessage
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MessagePassingTest extends AnyFlatSpec with MockFactory with Matchers {
  "An actor" should "play something when a sync message is received" in {
    val instrument = mock[Instrument]
    val orchestra = ActorSystem("orchestra")
    val behaviours = List(new SyncMessageReceivedBehaviour, new MusicMessageInfoReceivedBehaviour)
    val actorProps = Props(AIMusicianBuilder(instrument, behaviours, orchestra, None).build)
    val musician = TestActorRef.create(orchestra, actorProps, "MyMusician")

    (instrument.play _).expects(*).returning().once()
    // Directly injecting message in order to avoid concurrency issues
    musician.receive(new SyncMessage(musician, 1))
  }
}
