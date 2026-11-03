package training.segmentation

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import representation.{MusicalElement, Phrase, Rest, TimeSignature, TimeSignatureChange}

/** Where [[BarSplitTimesFinder]] cuts, on phrases that are just a stretch of time (#27). */
class BarSplitTimesFinderTest extends AnyFlatSpec with Matchers {
  private def ns(seconds: Double): BigInt = BigInt((seconds * 1e9).round)

  /** A phrase that lasts `seconds`. */
  private def lasting(seconds: Double): Phrase = Phrase(List(Rest(durationNS = ns(seconds))))

  private def change(beat: Int, numerator: Int, denominator: Int) =
    TimeSignatureChange(BigDecimal(beat), TimeSignature(numerator, denominator))

  /** The cuts of the finder as lengths in seconds of the phrases between them, at 120 BPM (a beat is half a second). */
  private def phraseSeconds(
      changes: Seq[TimeSignatureChange],
      lengthSeconds: Double,
      bars: Int = PhraseSegmenter.DEFAULT_BARS_PER_PHRASE,
      bpm: Double = 120.0): List[Double] =
    new BarSplitTimesFinder(changes, bpm, bars).getSplitTimes(lasting(lengthSeconds)).map(_.toDouble / 1e9).toList

  "The bar split times finder" should "cut every 2 bars of 4/4, 8 beats, when the time signature says 4/4" in {
    // 8 beats at 120 BPM are 4 seconds; 20 seconds hold five of them, and the cut at exactly 20 s is a complete window
    phraseSeconds(List(change(0, 4, 4)), 20) shouldBe List.fill(5)(4.0)
  }

  it should "assume 4/4 when there is no time signature at all" in {
    phraseSeconds(Nil, 20) shouldBe phraseSeconds(List(change(0, 4, 4)), 20)
  }

  it should "only cut where a whole window fits and drop what is left, like the 5 second cut does" in {
    phraseSeconds(Nil, 19.999) shouldBe List.fill(4)(4.0)
    phraseSeconds(Nil, 4.0) shouldBe List(4.0)
    // shorter than one window: no cut, the segmenter then returns the phrase whole
    phraseSeconds(Nil, 3.999) shouldBe Nil
  }

  it should "make a bar as long as its time signature says, in beats" in {
    // the beat is a quarter note, so at 120 BPM it lasts half a second
    phraseSeconds(List(change(0, 3, 4)), 12) shouldBe List.fill(4)(3.0) // 6 beats
    phraseSeconds(List(change(0, 2, 4)), 12) shouldBe List.fill(6)(2.0) // 4 beats
    phraseSeconds(List(change(0, 5, 4)), 25) shouldBe List.fill(5)(5.0) // 10 beats
    phraseSeconds(List(change(0, 2, 2)), 16) shouldBe List.fill(4)(4.0) // 2 half notes = 8 beats, like 4/4
    phraseSeconds(List(change(0, 6, 8)), 12) shouldBe List.fill(4)(3.0) // 6 eighths = 3 beats a bar, 6 beats a phrase
    phraseSeconds(List(change(0, 7, 8)), 14) shouldBe List.fill(4)(3.5) // 3.5 beats a bar
    phraseSeconds(List(change(0, 3, 16)), 6) shouldBe List.fill(8)(0.75) // 0.75 beat a bar
  }

  it should "cut every n bars, with n a parameter" in {
    phraseSeconds(Nil, 16, bars = 1) shouldBe List.fill(8)(2.0)
    phraseSeconds(Nil, 16, bars = 4) shouldBe List.fill(2)(8.0)
    phraseSeconds(Nil, 16, bars = 3) shouldBe List.fill(2)(6.0) // 12 beats; 16 s hold two of them
  }

  it should "measure the phrase in beats, whatever the tempo" in {
    phraseSeconds(Nil, 16, bpm = 60.0) shouldBe List.fill(2)(8.0)
    phraseSeconds(Nil, 16, bpm = 240.0) shouldBe List.fill(8)(2.0)
  }

  it should "cut on the bar lines of each section when the time signature changes, counting bars on across the change" in {
    // 3 bars of 4/4 (12 beats), then 3/4. Bars start at beats 0, 4, 8 | 12, 15, 18, 21, 24, 27, ...
    // Phrases (2 bars): beats 0-8, 8-15 (a 4/4 bar and the first 3/4 bar), 15-21, 21-27, 27-33
    phraseSeconds(List(change(0, 4, 4), change(12, 3, 4)), 33 * 0.5) shouldBe List(4.0, 3.5, 3.0, 3.0, 3.0)
    // 4 bars of 4/4, then 3/4: the change falls between two phrases
    phraseSeconds(List(change(0, 4, 4), change(16, 3, 4)), 28 * 0.5) shouldBe List(4.0, 4.0, 3.0, 3.0)
  }

  it should "start a new bar at a change that does not fall on a bar line of the previous signature" in {
    // 4/4 until beat 6, which is in the middle of the second bar: that bar ends there and 3/4 bars start at 6, 9, 12, ...
    // bar starts: 0, 4, 6, 9, 12, 15; phrases cut at bars 3 and 5: beats 6 and 12
    phraseSeconds(List(change(0, 4, 4), change(6, 3, 4)), 18 * 0.5) shouldBe List(3.0, 3.0, 3.0)
  }

  it should "use 4/4 until the first time signature event when that comes late, and let the event start a bar" in {
    // an anacrusis of 2 beats before the first event: bar 1 is those 2 beats, then 3/4 bars start at 2, 5, 8, 11
    // phrases cut at bars 3 and 5: beats 5 and 11
    phraseSeconds(List(change(2, 3, 4)), 11 * 0.5) shouldBe List(2.5, 3.0)
  }

  it should "ignore a repeated time signature (waterloo.mid says 4/4 twice) and keep the last of simultaneous events" in {
    // a second 4/4 at beat 6 does not start a bar there: the bar lines stay at 0, 4, 8, ...
    phraseSeconds(List(change(0, 4, 4), change(6, 4, 4)), 16) shouldBe List.fill(4)(4.0)
    // 3/4 and then 4/4 at beat 0: 4/4 holds
    phraseSeconds(List(change(0, 3, 4), change(0, 4, 4)), 16) shouldBe List.fill(4)(4.0)
  }

  it should "not depend on the order the time signatures come in" in {
    val changes = List(change(0, 4, 4), change(12, 3, 4), change(30, 2, 4))
    phraseSeconds(changes.reverse, 30) shouldBe phraseSeconds(changes, 30)
  }

  it should "keep the cuts exact when a beat is not a whole number of nanoseconds" in {
    // 149 BPM: a beat is 402,684,563.76 ns. Each cut is the exact bar line, truncated, never the sum of truncated windows
    val cuts = new BarSplitTimesFinder(Nil, 149.0).getSplitTimes(lasting(60)).toList
    cuts.scanLeft(BigInt(0))(_ + _).tail.zipWithIndex.foreach { case (cut, index) =>
      cut shouldBe MusicalElement.fromBPM(BigDecimal(8 * (index + 1)), 149.0)
    }
    cuts should have size 18 // 60 s are 149 beats: 18 windows of 8 beats
  }

  it should "not cut with a tempo it cannot use" in {
    new BarSplitTimesFinder(Nil, 0.0).getSplitTimes(lasting(60)) shouldBe Nil
    new BarSplitTimesFinder(Nil, -1.0).getSplitTimes(lasting(60)) shouldBe Nil
    new BarSplitTimesFinder(Nil, Double.NaN).getSplitTimes(lasting(60)) shouldBe Nil
  }

  it should "need at least one bar a phrase" in {
    an[IllegalArgumentException] should be thrownBy new BarSplitTimesFinder(Nil, 120.0, 0)
  }

  "The phrase segmenter by bars" should "give phrases that start on the bar lines" in {
    val segmenter = PhraseSegmenter.byBars(List(change(0, 3, 4)), 120.0)
    val segments = segmenter.segment(lasting(15.5))
    segments should have size 5 // 3 seconds each; the half second after the last bar line is dropped
    segments.foreach(_.getDurationNS shouldBe ns(3.0))
  }

  "The time based phrase segmenter" should "still cut every 5 seconds, or every length it is given" in {
    PhraseSegmenter.getDefault().segment(lasting(26)).map(_.getDurationNS) shouldBe List.fill(5)(ns(5.0))
    PhraseSegmenter.getDefault(ns(2.0)).segment(lasting(7)).map(_.getDurationNS) shouldBe List.fill(3)(ns(2.0))
  }
}
