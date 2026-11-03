package actors.musicians

import org.apache.pekko.actor.Actor
import representation.Phrase


trait Musician extends Actor {
  def play(phrase: Phrase): Unit
}
