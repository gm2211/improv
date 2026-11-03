package genetic

import cbr.MusicalCase
import instruments.InstrumentType.PIANO
import org.scalatest.flatspec.AnyFlatSpec
import representation.{Jazz, Note, Phrase}

import scala.concurrent.duration.MILLISECONDS

class SimilarityMeasureTest extends AnyFlatSpec {

  private def intDistance: (Int, Int) => Double = (a, b) => scala.math.abs(a - b).toDouble

  private def phraseOf(pitches: Seq[Int]): MusicalCase = {
    val noteDurationNS = MILLISECONDS.toNanos(200)
    val notes = pitches.zipWithIndex.map { case (pitch, idx) =>
      Note(midiPitch = pitch, durationNS = noteDurationNS, startTimeNS = noteDurationNS * idx)
    }
    MusicalCase(PIANO(1), Jazz, Phrase().withMusicalElements(notes))
  }

  "distanceToSimilarity" should "map a distance of zero to a similarity of one" in {
    assert(SimilarityMeasures.distanceToSimilarity(0.0) == 1.0)
  }

  it should "give a smaller similarity to a bigger distance, always within (0, 1]" in {
    val distances = List(0.0, 0.1, 1.0, 5.0, 100.0, 1e6, 1e12)
    val similarities = distances.map(SimilarityMeasures.distanceToSimilarity)

    similarities.foreach(s => assert(s > 0.0 && s <= 1.0, s"$s is not in (0, 1]"))
    similarities.zip(similarities.tail).foreach { case (closer, further) => assert(closer > further) }
  }

  "fromDistance" should "make the nearest element the most similar one" in {
    val similarity = SimilarityMeasures.fromDistance(intDistance)

    assert(similarity(10, 10) == 1.0)
    assert(similarity(10, 11) > similarity(10, 20))
    assert(similarity(10, 20) > similarity(10, 200))
  }

  "euclidean" should "rate identical signatures as 1 and nearer signatures as more similar than farther ones" in {
    val similarity = SimilarityMeasures.euclidean[Array[Double]](identity)
    val reference = Array(1.0, 2.0, 3.0)

    assert(similarity(reference, reference.clone()) == 1.0)
    assert(similarity(reference, Array(1.0, 2.0, 4.0)) > similarity(reference, Array(5.0, 9.0, 12.0)))
    assert(similarity(reference, Array(1.0, 2.0, 4.0)) == 1.0 / (1.0 + 1.0))
  }

  "The musical case similarity" should "rate a case as maximally similar to itself" in {
    val case1 = phraseOf(Seq(60, 62, 64, 65, 67))

    assert(SimilarityMeasures.musicalCaseSimilarity(case1, case1) == 1.0)
  }

  it should "rate a case that is close to the reference higher than one that is far from it" in {
    val reference = phraseOf(Seq(60, 62, 64, 65, 67))
    val close = phraseOf(Seq(60, 62, 64, 65, 69))
    val far = phraseOf(Seq(30, 90, 20, 100, 10, 120, 5, 127))

    val similarity = SimilarityMeasures.musicalCaseSimilarity
    assert(similarity(reference, close) > similarity(reference, far))
    assert(similarity(reference, far) > 0.0)
  }

  "The neutral measure" should "rate everything as equally similar" in {
    val similarity = SimilarityMeasures.neutral[Int]

    assert(similarity(1, 1) == 1.0)
    assert(similarity(1, 1000) == 1.0)
  }

  "A GASelector" should "score a candidate higher the more it resembles the previous solution, at equal fitness" in {
    val selector = new GASelector[Int, Int](
      fitnessFunction = _ => 1.0,
      chromosomeGenerator = identityChromosomes,
      geneticOperator = averagingCrossover,
      similarityMeasure = SimilarityMeasures.fromDistance(intDistance))
    val previous = Some(10)

    val scores = List(10, 11, 15, 40, 200).map(selector.score(_, previous))

    scores.zip(scores.tail).foreach { case (closer, further) => assert(closer > further) }
  }

  it should "weigh the fitness by the similarity rather than ignore either" in {
    // 40 is 30 away from the previous solution (similarity 1/31, which is about 6e-6 once raised to 3.5)
    def selectorWithFitnessOf40(fitnessOf40: Double) = new GASelector[Int, Int](
      fitnessFunction = Map(10 -> 1.0, 40 -> fitnessOf40),
      chromosomeGenerator = identityChromosomes,
      geneticOperator = averagingCrossover,
      similarityMeasure = SimilarityMeasures.fromDistance(intDistance))

    // a big enough fitness advantage outweighs the lack of similarity...
    assert(selectorWithFitnessOf40(1e7).score(40, Some(10)) > selectorWithFitnessOf40(1e7).score(10, Some(10)))
    // ...but a modest one does not
    assert(selectorWithFitnessOf40(1000.0).score(40, Some(10)) < selectorWithFitnessOf40(1000.0).score(10, Some(10)))
    // the same candidate scores lower as the previous solution moves away from it
    assert(selectorWithFitnessOf40(1.0).score(40, Some(40)) > selectorWithFitnessOf40(1.0).score(40, Some(10)))
  }

  it should "ignore similarity when there is no previous solution" in {
    val selector = new GASelector[Int, Int](
      fitnessFunction = _.toDouble,
      chromosomeGenerator = identityChromosomes,
      geneticOperator = averagingCrossover,
      similarityMeasure = SimilarityMeasures.fromDistance(intDistance))

    assert(selector.score(7, None) == 7.0)
  }

  it should "work with the default similarity measure when there is a previous solution" in {
    val selector = new GASelector[Int, Int](
      fitnessFunction = _.toDouble,
      chromosomeGenerator = identityChromosomes,
      geneticOperator = averagingCrossover)

    val selected = selector.selectSolution(Some(3), List(1, 2, 3, 4, 5, 6), Nil)

    assert(selected.exists((1 to 6).contains))
  }

  private def identityChromosomes: ChromosomeGenerator[Int, Int] = new ChromosomeGenerator[Int, Int] {
    override def createChromosome(elem: Int): Int = elem
    override def fromChromosome(chromosome: Int): Int = chromosome
  }

  private def averagingCrossover: GeneticOperator[Int] = new GeneticOperator[Int] {
    override def crossOver(e1: Int, e2: Int): Int = (e1 + e2) / 2
  }
}
