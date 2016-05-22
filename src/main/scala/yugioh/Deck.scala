package yugioh

import java.util
import java.util.Collections

import yugioh.card.Card
import yugioh.Util.intWithTimes
import yugioh.action.Action
import yugioh.card.state.{FieldState, HandState}

import scala.collection.mutable.ListBuffer

trait Deck {
  protected val cards: ListBuffer[Card]

  val owner: Player

  def shuffle(): Unit = Collections.shuffle(util.Arrays.asList(cards: _*))

  def fromTop(): Card = fromTop(1).head

  /**
    * An EmptyDeck GameLoss exception will be thrown if the deck is empty.
    */
  def fromTop(howMany: Int): Seq[Card] = {
    try {
      for (_ <- 1 to howMany) yield cards.remove(0)
    } catch {
      case outOfBounds: IndexOutOfBoundsException => throw EmptyDeckImpl
    }
  }
}

class DeckImpl(val owner: Player, val cards: ListBuffer[Card]) extends Deck {
  def this(owner: Player) {
    // TODO initialize with more than empty
    this(owner, new ListBuffer[Card])
  }
}

class TestDeck(val owner: Player) extends Deck {
  val cards = new ListBuffer[Card]

  60 times {
    cards.append(new TestCard(owner))
  }

  class TestCard(val owner: Player) extends Card {
    override val printedName: String = "Test Card"

    override def location: Location = InDeck

    override def sendToGrave()(implicit gameState: GameState): Unit = {}

    override def actions(implicit gameState: GameState): Seq[Action] = Seq.empty

    override def banish()(implicit gameState: GameState): Unit = {}

    override def fieldState: Option[FieldState] = None

    override def destroy()(implicit gameState: GameState): Unit = {}

    override def toHand()(implicit gameState: GameState): Unit = {}

    override def controller: Player = null

    override def toDeck()(implicit gameState: GameState): Unit = {}

    override def handState: Option[HandState] = None
  }
}
