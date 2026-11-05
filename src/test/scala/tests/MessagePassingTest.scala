package tests

import _root_.instruments.Instrument
import _root_.utils.ImplicitConversions._
import actors.musicians.AIMusicianBuilder
import actors.musicians.behaviour.{GenreSelectionBehaviour, MusicMessageInfoReceivedBehaviour, SyncMessageReceivedBehaviour}
import org.apache.pekko.actor.{ActorSystem, Props}
import org.apache.pekko.testkit.{TestActorRef, TestKit, TestProbe}
import messages.SyncMessage
import messages.consensus.{FinalDecision, GenreElection, GenrePreference, VoteRequest}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import representation.Jazz

class MessagePassingTest extends AnyFlatSpec with MockFactory with Matchers {
  /**
   * A musician only plays once it has a genre (`SyncMessageReceivedBehaviour`), and it gets one through the genre
   * election of `GenreSelectionBehaviour`: without that behaviour it never plays (#15).
   */
  "An actor" should "play something when a sync message is received" in {
    val instrument = mock[Instrument]
    val orchestra = ActorSystem("orchestra")
    try {
      val director = TestProbe()(using orchestra)
      val behaviours =
        List(new SyncMessageReceivedBehaviour, new MusicMessageInfoReceivedBehaviour, new GenreSelectionBehaviour)
      val actorProps = Props(AIMusicianBuilder(instrument, behaviours, orchestra, None).build)
      val musician = TestActorRef.create(orchestra, actorProps, "MyMusician")

      (instrument.play).expects(*).returning(()).once()
      // Directly injecting messages in order to avoid concurrency issues
      // 1. no genre yet: the musician does not play, it asks the director (the sender of the sync message) for an election
      musician.receive(SyncMessage(director.ref, 1))
      director.expectMsg(VoteRequest(musician, GenreElection))
      // 2. the director's decision gives it a genre, 3. so it plays on the next sync message
      musician.receive(FinalDecision(director.ref, GenrePreference(Jazz)))
      musician.receive(SyncMessage(director.ref, 2))
    } finally {
      TestKit.shutdownActorSystem(orchestra)
    }
  }
}
