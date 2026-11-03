package representation

import org.scalatest.flatspec.AnyFlatSpec
import representation.NoteName._

class PhraseTest extends AnyFlatSpec {

  "Notes across the split time" should "be split among the two resulting phrases" in {
    import scala.concurrent.duration.SECONDS
    val p = Phrase()
      .withMusicalElements(
        Phrase().withMusicalElements(Note(A).withStartTime(0, SECONDS).withDuration(1, SECONDS),
                                     Note(D).withStartTime(1, SECONDS).withDuration(2, SECONDS),
                                     Note(NoteName.F).withStartTime(3, SECONDS).withDuration(1, SECONDS)),
        Phrase().withMusicalElements(Note(B).withStartTime(0, SECONDS).withDuration(2, SECONDS),
                                     Note(E).withStartTime(2, SECONDS).withDuration(2, SECONDS)),
        Phrase().withMusicalElements(Note(C).withStartTime(0, SECONDS).withDuration(4, SECONDS))
      )
      .withPolyphony(polyphony = true).get
    val (p1, p2) = Phrase.split(p, SECONDS.toNanos(1))
    val bSplitP1 = p1.get.musicalElements(1).asInstanceOf[Phrase].musicalElements.head.asInstanceOf[Note]
    val bSplitP2 = p2.get.musicalElements(1).asInstanceOf[Phrase].musicalElements.head.asInstanceOf[Note]

    assert(bSplitP1.name.equals(B))
    assert(bSplitP1.getDuration(SECONDS) == BigInt(1))
    assert(bSplitP1.getStartTime(SECONDS) == BigInt(0))

    assert(bSplitP2.name.equals(B))
    assert(bSplitP2.getDuration(SECONDS) == BigInt(1))
    assert(bSplitP2.getStartTime(SECONDS) == BigInt(0))
  }
}
