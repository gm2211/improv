package demos

import actors.Orchestra
import actors.composers.{CBRComposer, MIDIReaderComposer, RandomComposer}
import actors.directors.WaitingDirector
import actors.musicians.AIMusician
import actors.musicians.AIMusician._
import cbr.MusicalCase
import instruments.InstrumentType._
import instruments.JFugueInstrument
import midi.JMusicMIDIParser
import storage.KDTreeIndex
import utils.UserInput

object DemoMIDIOrchestra {
  def main(args: Array[String]): Unit = run()

  private def createComposer(composerType: String)(filename: String, partNumber: Int) = {
    composerType match {
      case "rand" =>
        new RandomComposer
      case "midi" =>
        MIDIReaderComposer.builder
          .withFilename(filename)
          .withPartNum(partNumber)
          .withMIDIParser(JMusicMIDIParser)
          .build
      case "cbr" =>
        val index = KDTreeIndex.loadDefault[MusicalCase].get
        new CBRComposer(index)
    }
  }

  def run() = {
    val orchestra = Orchestra.builder
      .withDirector(WaitingDirector.builder)
      .build

//    val filename = UserInput.chooseASong("musicScores")
    val filename = UserInput.chooseASong()

    val parser = JMusicMIDIParser(filename)

    val musicianBuilder = (instrType: InstrumentType, partNumber: Int) => {
      val instrument = new JFugueInstrument(instrType)
      val composer = createComposer("midi")(filename, partNumber)

      AIMusician.builder
        .withInstrument(instrument)
        .withComposer(composer)
//        .isMessageOnly
    }

    for (((instrument, parts), idx) <- parser.getPartIndexByInstrument.zipWithIndex if idx < 7) {
      parts.map(musicianBuilder(instrument, _))
        .foreach(m => orchestra.registerMusician(m))
    }

//        orchestra.registerMusician(new JFugueSynchronizedPlayer)

    orchestra.start()
  }
}
