package yugioh

import java.util
import java.util.Collections

import yugioh.card.Card

import scala.collection.mutable.ListBuffer

trait Deck {
  protected val cards: ListBuffer[Card]
  def owner: Player

  def shuffle(): Unit = Collections.shuffle(util.Arrays.asList(cards: _*))
  def fromTop(): Card = cards.remove(0)
}

class DeckImpl(val owner: Player, val cards: ListBuffer[Card]) extends Deck {
  def this(owner: Player) {
    // TODO initialize with more than empty
    this(owner, new ListBuffer[Card])
  }
}
