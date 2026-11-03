package instruments

import instruments.InstrumentType.PIANO
import org.scalatest.flatspec.AnyFlatSpec
import representation.{MusicalElement, Note, NoteName, Phrase}

class JFugueUtilsTest extends AnyFlatSpec {
  
  "A polyphonic phrase" should "be converted into a pattern that uses @<time_val>" in {
    val d1 = MusicalElement.fromBPM(0.5)
    val d2= MusicalElement.fromBPM(1)

    val phrases = List(
      List(new Note(NoteName.A, durationNS = d1), new Note(NoteName.D, durationNS = d2)),
      List(new Note(NoteName.B, durationNS = d1), new Note(NoteName.E, durationNS = d2)),
      List(new Note(NoteName.C, durationNS = d1), new Note(NoteName.F, durationNS = d2))
    ).map(nl => new Phrase(nl))
    val polyphonicPhrase = new Phrase(musicalElements = phrases.toList, polyphony = true)

    val pattern = JFugueUtils.createPattern(polyphonicPhrase, PIANO(1))
    println(pattern)
    assert(pattern.toString == "I[Piano] @0.0 A4h @0.0 B4h @0.0 C4h @0.5 D4w @0.5 E4w @0.5 F4w")
  }

}
