package genetic

import cbr.MusicalCase
import instruments.InstrumentType.{BASS, PIANO}
import org.scalatest.flatspec.AnyFlatSpec
import representation.{Blues, Jazz, Note, Phrase}

import scala.concurrent.duration.SECONDS

class MusicalCaseCrossoverOperatorTest extends AnyFlatSpec {
  private val NoteLengthNS: BigInt = SECONDS.toNanos(1)

  /** One note of a second each, back to back */
  private def phraseOf(pitches: Seq[Int]): Phrase = Phrase().withMusicalElements(
    pitches.zipWithIndex.map { case (pitch, idx) =>
      Note(midiPitch = pitch, durationNS = NoteLengthNS, startTimeNS = NoteLengthNS * idx)
    })

  private def caseOf(pitches: Seq[Int]): MusicalCase = MusicalCase(PIANO(1), Jazz, phraseOf(pitches))

  private def notesOf(case1: MusicalCase): List[Note] = case1.phrase.musicalElements.collect { case n: Note => n }

  private def pitchesOf(case1: MusicalCase): List[Int] = notesOf(case1).map(_.midiPitch)

  private val crossover = new MusicalCaseCrossoverOperator

  private val firstParent = caseOf(Seq(60, 61, 62, 63, 64))
  private val secondParent = caseOf(Seq(70, 71, 72, 73, 74))

  "The crossover" should "start from the first segment of the first parent" in {
    assert(pitchesOf(crossover.crossOver(firstParent, secondParent)).head == 60)
  }

  it should "start from the first segment of the second parent when that one is the first argument" in {
    assert(pitchesOf(crossover.crossOver(secondParent, firstParent)).head == 70)
  }

  it should "alternate the segments of the two parents, using every segment" in {
    assert(pitchesOf(crossover.crossOver(firstParent, secondParent)) == List(60, 71, 62, 73, 64))
    assert(pitchesOf(crossover.crossOver(secondParent, firstParent)) == List(70, 61, 72, 63, 74))
  }

  it should "lay the segments out one after the other" in {
    val child = crossover.crossOver(firstParent, secondParent)

    assert(notesOf(child).map(_.startTimeNS) == (0 until 5).map(NoteLengthNS * _).toList)
    assert(child.phrase.getDurationNS == NoteLengthNS * 5)
  }

  it should "use the first segments of the longer parent when the parents differ in length" in {
    val longerParent = caseOf(Seq(70, 71, 72, 73, 74, 75, 76, 77, 78, 79))

    assert(pitchesOf(crossover.crossOver(firstParent, longerParent)) == List(60, 71, 62, 73, 64))
    assert(pitchesOf(crossover.crossOver(longerParent, firstParent)) == List(70, 61, 72, 63, 74))
  }

  it should "give the child the instrument and the genre of one of its parents" in {
    val piano = MusicalCase(PIANO(1), Jazz, phraseOf(Seq(60, 61, 62, 63, 64)))
    val bass = MusicalCase(BASS(33), Blues, phraseOf(Seq(40, 41, 42, 43, 44)))

    val children = List.fill(50)(crossover.crossOver(piano, bass))

    assert(children.forall(c => c.instrumentType == piano.instrumentType || c.instrumentType == bass.instrumentType))
    assert(children.forall(c => c.genre == Jazz || c.genre == Blues))
    assert(children.exists(_.instrumentType == piano.instrumentType))
    assert(children.exists(_.instrumentType == bass.instrumentType))
  }

  it should "not shrink the phrases it is applied to over generations" in {
    val grandchild = (1 to 10).foldLeft(firstParent) { (child, _) => crossover.crossOver(child, secondParent) }

    assert(grandchild.phrase.getDurationNS == NoteLengthNS * 5)
    assert(notesOf(grandchild).size == 5)
  }

  it should "not fail when a phrase is too short to be cut into segments" in {
    val blip = MusicalCase(PIANO(1), Jazz, Phrase().withMusicalElements(Note(durationNS = 3)))
    val silence = MusicalCase(PIANO(1), Jazz, Phrase())

    assert(crossover.crossOver(blip, secondParent).phrase.nonEmpty)
    assert(crossover.crossOver(firstParent, blip).phrase.nonEmpty)
    assert(crossover.crossOver(silence, secondParent).phrase.isEmpty)
    assert(crossover.crossOver(firstParent, silence).phrase.nonEmpty)
  }
}
