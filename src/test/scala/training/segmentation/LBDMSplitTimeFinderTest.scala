package training.segmentation

import midi.JMusicMIDIParser
import org.scalatest.flatspec.AnyFlatSpec
import utils.{NumericUtils, IOUtils}
import utils.collections.CollectionUtils

class LBDMSplitTimeFinderTest extends AnyFlatSpec {

  "The split times finder" should "return a list of split times" in {

    val songTitle: String = "furElise"
    val filename = IOUtils.getResourcePath(s"musicScores/$songTitle.mid")
    //    val filename = IOUtils.getResourcePath("musicScores/piano.mid")
    //  val filename = IOUtils.getResourcePath("musicScores/test.mid")
    val p1 = JMusicMIDIParser(filename)
    val phraseSegmenter = new LBDMSplitTimeFinder(false) with PhraseSegmenter
    val p2 = JMusicMIDIParser(phraseSegmenter)(filename)

    val phrases1 = p1.getMultiVoicePhrases(0)
    val phrases2 = p2.getMultiVoicePhrases(0)

    val phrase = p1.getMultiVoicePhrase(0)

    val splitTimes = phraseSegmenter.getSplitTimes(phrase.get)
    val pitchProfile = NumericUtils.normalise(phraseSegmenter.computePitchProfile(phrase.get))
    val ioiProfile = NumericUtils.normalise(phraseSegmenter.computeIOIProfile(phrase.get))
    val restsProfile = NumericUtils.normalise(phraseSegmenter.computeRestsProfile(phrase.get))
    val combinedProfile = NumericUtils.normalise(phraseSegmenter.computeProfile(phrase.get))

    CollectionUtils.print(splitTimes)
    CollectionUtils.print(pitchProfile)
    CollectionUtils.print(ioiProfile)
    CollectionUtils.print(restsProfile)
    CollectionUtils.print(combinedProfile)
    val peaks = NumericUtils.findPeaks(combinedProfile.toList)
    val avgPeak = NumericUtils.avg(peaks.map(_._1))
    val end = List(pitchProfile.size, ioiProfile.size, restsProfile.size).max

    assert(splitTimes.nonEmpty)
    assert(peaks.nonEmpty)
    assert(avgPeak.isNaN == false)
    assert(end > 0)
  }

}
