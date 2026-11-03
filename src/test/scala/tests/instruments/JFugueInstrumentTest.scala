package tests.instruments

import designPatterns.observer.{EventNotification, Observer}
import instruments.InstrumentType.PIANO
import instruments.{AsyncInstrument, JFugueInstrument}
import org.scalatest.flatspec.AnyFlatSpec
import representation.{MusicalElement, Note, Phrase, Rest}
import tests.TestTags.SlowTest

import scala.collection.mutable.ListBuffer

class JFugueInstrumentTest extends AnyFlatSpec {
  var instrument: JFugueInstrument = _
  val lock = new AnyRef

  def setup() = {
    instrument = new JFugueInstrument(PIANO())
  }

  "An instrument" should "notify correctly when it is done playing" taggedAs SlowTest in {
    setup()
    val musicalElements = ListBuffer[MusicalElement]()
      musicalElements.append(Note.fromString("A5").withDuration(1))
      musicalElements.append(new Rest(MusicalElement.fromBPM(0.1)))
      musicalElements.append(Note.fromString("B5").withDuration(1))
      musicalElements.append(new Rest(MusicalElement.fromBPM(0.1)))
      musicalElements.append(Note.fromString("C5").withDuration(1))

    val phrase = new Phrase().withMusicalElements(musicalElements)

    val listener = new Observer {
      override def notify(eventNotification: EventNotification): Unit = eventNotification match {
        case AsyncInstrument.FinishedPlaying =>
          println("FinishedPlaying")
          lock.synchronized(lock.notifyAll())
      }
    }
    instrument.addObserver(listener)
    instrument.play(phrase)
    while (instrument.player.playing) {
      lock.synchronized(lock.wait())
    }
  }
}
