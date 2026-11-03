package actors.composers

import cbr.MusicalCase
import instruments.InstrumentType.InstrumentType
import representation.{Note, Phrase}

class RandomComposer extends Composer {
  override def compose(
      previousPhrase: Option[MusicalCase] = None,
      phrasesByOthers: Iterable[MusicalCase],
      constraints: List[MusicalCase => Boolean]): Option[Phrase] = {
    val range = 1 to 5
    val phrase = Phrase()
      .withMusicalElements(range.map(i => Note.genRandNote().withDurationBPM(0.5, 120.0)))
    Some(phrase)
  }
}
