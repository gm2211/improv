package demos

import actors.Orchestra
import actors.composers.CBRComposer
import actors.directors.WaitingDirector
import actors.musicians.AIMusician
import actors.musicians.AIMusician._
import cbr.MusicalCase
import genetic.PhraseSelectors
import instruments.InstrumentType._
import instruments.{Instrument, JFugueInstrument}
import storage.KDTreeIndex

import scala.util.Try

object DemoCBROrchestra {
  def main(args: Array[String]): Unit = run()

  def getRandInstrument: Instrument = new JFugueInstrument

  val getInstrument =
    List(PIANO(), BASS(), BRASS(), PERCUSSIVE(), GUITAR(), ORGAN()).iterator

  def run(): Unit = {
    val director = WaitingDirector.builder
    val orchestra = Orchestra.builder.withDirector(director).build
    val numMusicians = 20

    def musicianBuilder = {
      val composer = new CBRComposer(KDTreeIndex.loadDefault[MusicalCase].get, Some(PhraseSelectors.getGASelector))
      val instrument: Try[Instrument] = Try(new JFugueInstrument(getInstrument.next())) //Try(getRandInstrument)

      AIMusician.builder
        .withInstrument(instrument.getOrElse(getRandInstrument))
        .withComposer(composer)
    }

    (1 to numMusicians).foreach(_ =>orchestra.registerMusician(musicianBuilder))
    orchestra.start()
  }
}
