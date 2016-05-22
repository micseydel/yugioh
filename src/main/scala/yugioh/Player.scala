package yugioh

import yugioh.action.Action
import yugioh.card.Card

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

trait Player {
  val name: String
  val deck: Deck
  val field: Field = new FieldImpl
  val grave: ListBuffer[Card] = new ListBuffer[Card]
  val banished: ListBuffer[Card] = new ListBuffer[Card]
  val hand: ListBuffer[Card] = new ListBuffer[Card]

  var lifePoints: Int = Constants.InitialLifePoints

  /**
    * @param actions must contain at least one thing, typically an option to pass
    */
  def chooseAction(actions: Seq[Action])(implicit gameState: GameState, phase: Phase): Action

  /**
    * Called when MP1 is ended.
    *
    * @return true to enter BP, otherwise go to EP
    */
  def enterBattlePhase: Boolean = false

  def draw(): Unit = draw(1)
  def draw(howMany: Int): Unit = hand ++= deck.fromTop(howMany)

  def cardToDiscardForHandSizeLimit: Card

  /**
    * Ask player if they wish to end the phase or step.
    */
  def consentToEnd(implicit phase: Phase, maybeStep: Option[Step]): Boolean

  private[this] var myOpponent: Player = SentinelOpponent

  def opponent_=(player: Player) = {
    if (myOpponent != SentinelOpponent) {
      throw new IllegalStateException(s"Opponent has already been set once (to $myOpponent), cannot be set a second time.")
    }

    myOpponent = player
  }

  def opponent = {
    if (myOpponent == SentinelOpponent) {
      throw new IllegalStateException("Opponent has not been set yet.")
    }

    myOpponent
  }
}

/**
  * A player used as a default value, trying to do anything with it will result in an UnsupportedOperationException.
  */
private object SentinelOpponent extends Player {
  private def fail = throw new UnsupportedOperationException("Opponent was not set.")

  override val name = null
  override val deck = null
  override def consentToEnd(implicit phase: Phase, maybeStep: Option[Step]) = fail
  override def cardToDiscardForHandSizeLimit = fail
  override def chooseAction(actions: Seq[Action])(implicit gameState: GameState, phase: Phase) = fail
  override def opponent = fail
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

  override def enterBattlePhase = {
    print("MP1 is ending; enter BP? (If not, will go to EP) ")
    StdIn.readBoolean()
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
    var choice = StdIn.readInt()
    while (choice < 0 || choice >= options.size) {
      println("Not a valid option.")
      print("> ")
      choice = StdIn.readInt()
    }

    options(choice)
  }

  override def consentToEnd(implicit phase: Phase, maybeStep: Option[Step]): Boolean = {
    println(s"End $phase?")
    StdIn.readBoolean()
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
  override def consentToEnd(implicit phase: Phase, maybeStep: Option[Step]): Boolean = true
}
