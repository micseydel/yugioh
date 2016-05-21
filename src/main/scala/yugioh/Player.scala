package yugioh

import yugioh.action.Action
import yugioh.card.Card

import scala.collection.mutable.ListBuffer

trait Player {
  val name: String
  val deck: Deck
  val field: Field = new FieldImpl
  val grave: ListBuffer[Card] = new ListBuffer[Card]
  val banished: ListBuffer[Card] = new ListBuffer[Card]
  val hand: ListBuffer[Card] = new ListBuffer[Card]

  var lifePoints: Int = Player.InitialLifePoints

  /**
    * @param actions must contain at least one thing, typically an option to pass
    */
  def chooseAction(actions: Seq[Action])(implicit gameState: GameState, phase: Phase): Action

  def draw(): Unit = hand.append(deck.fromTop())

  def cardToDiscardForHandSizeLimit: Card
}

object Player {
  val InitialLifePoints = 8000
}

class CommandLineHumanPlayer(val name: String) extends Player {
  override val deck: Deck = new TestDeck(this) // TODO: be more than just a stub

  override def chooseAction(actions: Seq[Action])(implicit gameState: GameState, phase: Phase) = {
    if (actions.size == 1) {
      val action = actions.head
      println(s"Action $action was only option, taking implicitly.")
      action
    } else {
      select("Select an action:", actions)
    }
  }

  override def cardToDiscardForHandSizeLimit: Card = {
    select("Select a card to discard:", hand)
  }

  /**
    * Ask the user for a specific element of a sequence.
    */
  def select[A](prompt: String, options: Seq[A]): A = {
    println(prompt)

    for ((action, i) <- options.zipWithIndex) {
      println(s"($i) $action")
    }

    print("> ")
    var choice = scala.io.StdIn.readInt()
    while (choice < 0 || choice >= options.size) {
      println("Not a valid option.")
      print("> ")
      choice = scala.io.StdIn.readInt()
    }

    options(choice)
  }
}

/**
  * Useful for testing purposes.
  *
  * When given an option like discarding for hand size or choosing an action, will just select the first.
  */
class PassivePlayer extends Player {
  override val name = "PassivePlayer"
  override val deck = new TestDeck(this)
  override def cardToDiscardForHandSizeLimit: Card = hand.head
  override def chooseAction(actions: Seq[Action])(implicit gameState: GameState, phase: Phase) = actions.head
}
