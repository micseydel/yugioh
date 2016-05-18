package yugioh

import yugioh.card.Card

import scala.collection.mutable.ListBuffer

trait Player {
  val name: String
  val deck: Deck
  val field: Field
  val grave: ListBuffer[Card] = new ListBuffer[Card]
  val banished: ListBuffer[Card] = new ListBuffer[Card]
  val hand: ListBuffer[Card] = new ListBuffer[Card]
  val opponent: Player // TODO I don't like this

  var lifePoints: Int = Player.InitialLifePoints
  def draw(): Unit = hand.append(deck.fromTop())
}

object Player {
  val InitialLifePoints = 8000
}

/**
  * TODO: this should not be abstract
  */
abstract class HumanPlayer(val name: String) extends Player {

}

/**
  * Useful for testing purposes.
  * TODO: this should not be abstract
  */
abstract class PassivePlayer() extends Player {
  override val name = "PassivePlayer"
  // TODO useless deck
}
