package actors.composers

import cbr.MusicalCase
import instruments.InstrumentType.InstrumentType
import representation.Phrase

trait Composer {
  def compose(
    previousSolution: Option[MusicalCase] = None,
    phrasesByOthers: Iterable[MusicalCase],
    constraints: List[MusicalCase => Boolean]): Option[Phrase]
}
