package tests

import java.io.FileOutputStream
import javax.sound.midi.MidiSystem

import _root_.instruments.InstrumentType.PIANO
import _root_.instruments.JFugueUtils
import _root_.midi.{JMusicMIDIParser, JMusicParserUtils}
import _root_.utils.{IOUtils, ProfilingUtils}
import org.scalatest.flatspec.AnyFlatSpec
import representation.Phrase

class JMusicMIDIParserTest extends AnyFlatSpec {
  val resourcePath = IOUtils.getResourcePath("musicScores/shortPiano.mid")
  val parser = JMusicMIDIParser(resourcePath)

  "The midi parser" should "return a list of parts and their indexes grouped by instrument" in {
    val partsIndices = parser.getPartIndexByInstrument.get(PIANO(1)).get
    assert(partsIndices == Set(0, 1))
  }

  "The midi parser" should "correctly assemble a phrase" in {
    println()
    val multiVoicePhrase = JMusicParserUtils.convertPart(parser.score.getPart(0)).getOrElse(Phrase())
    val mergedPhrase = Phrase.mergePhrases(multiVoicePhrase)
    val splitTask = () => Phrase.unmerge(mergedPhrase)

    val (timeElapsed, multiVoicePhraseRecovered) = ProfilingUtils.timeIt(splitTask(), 1)
    println(s"\nOn average it took $timeElapsed milliseconds to get $multiVoicePhraseRecovered")
    val theirSequence = JFugueUtils.toSequence(JFugueUtils.createPattern(multiVoicePhrase, PIANO(1)))
    val mySequence = JFugueUtils.toSequence(JFugueUtils.createPattern(multiVoicePhraseRecovered, PIANO(1)))
    val mergedSequence = JFugueUtils.toSequence(JFugueUtils.createPattern(mergedPhrase, PIANO(1)))
    val out1 = new FileOutputStream(IOUtils.getResourcePath("musicScores") + "/theirPiano.mid")
    val out2 = new FileOutputStream(IOUtils.getResourcePath("musicScores") + "/recovered.mid")
    val out3 = new FileOutputStream(IOUtils.getResourcePath("musicScores") + "/merged.mid")
    MidiSystem.write(theirSequence, 1, out1)
    MidiSystem.write(mySequence, 1, out2)
    MidiSystem.write(mergedSequence, 1, out3)
  }
}
