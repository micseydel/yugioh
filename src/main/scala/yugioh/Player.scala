package yugioh

import yugioh.action.Action
import yugioh.card.Card
import yugioh.card.monster.ExtraDeckMonster

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

trait Player {
  val name: String
  val deck: Deck
  val extraDeck: ListBuffer[ExtraDeckMonster] = new ListBuffer[ExtraDeckMonster]
  val field: Field = new FieldImpl
  val grave: ListBuffer[Card] = new ListBuffer[Card]
  val banished: ListBuffer[Card] = new ListBuffer[Card]
  val hand: ListBuffer[Card] = new ListBuffer[Card]

  var lifePoints: Int = Constants.InitialLifePoints

  /**
    * @param actions must contain at least one thing, typically an option to pass
    */
  def chooseAction(actions: Seq[Action])(implicit gameState: GameState, turnPlayer: Player, phase: Phase, step: Step = null): Action

  /**
    * Called when MP1 is ended.
    *
    * @return true to enter BP, otherwise go to EP
    */
  def enterBattlePhase(implicit gameState: GameState, turnPlayer: Player, phase: Phase, step: Step=null): Boolean

  def draw(): Unit = draw(1)
  def draw(howMany: Int): Unit = hand ++= deck.fromTop(howMany)

  def cardToDiscardForHandSizeLimit(implicit gameState: GameState, turnPlayer: Player, phase: Phase, step: Step): Card

  /**
    * Ask player if they wish to end the phase or step.
    */
  def consentToEnd(implicit gameState: GameState, turnPlayer: Player, phase: Phase, step: Step = null): Boolean

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

  override def toString = name
}

/**
  * A player used as a default value, trying to do anything with it will result in an UnsupportedOperationException.
  */
private object SentinelOpponent extends Player {
  private def fail = throw new UnsupportedOperationException("Opponent was not set for a player.")

  override val name = null
  override val deck = null
  override def opponent = fail
  override def consentToEnd(implicit gameState: GameState, turnPlayer: Player, phase: Phase, step: Step = null) = fail
  override def cardToDiscardForHandSizeLimit(implicit gameState: GameState, turnPlayer: Player, phase: Phase, step: Step = null) = fail
  override def chooseAction(actions: Seq[Action])(implicit gameState: GameState, turnPlayer: Player, phase: Phase, step: Step = null) = fail
  override def enterBattlePhase(implicit gameState: GameState, turnPlayer: Player, phase: Phase, step: Step = null): Boolean = fail
}

class CommandLineHumanPlayer(val name: String) extends Player {
  override val deck: Deck = new TestDeck(this) // TODO: be more than just a stub

  override def chooseAction(actions: Seq[Action])(implicit gameState: GameState, turnPlayer: Player, phase: Phase, step: Step) = {
    if (actions.size == 1) {
      val action = actions.head
      println(s"Action $action was only option (during phase $phase), taking implicitly.")
      action
    } else {
      select("Select an action:", actions)
    }
  }

  override def cardToDiscardForHandSizeLimit(implicit gameState: GameState, turnPlayer: Player, phase: Phase, step: Step): Card = {
    select("Select a card to discard:", hand)
  }

  override def enterBattlePhase(implicit gameState: GameState, turnPlayer: Player, phase: Phase, step: Step) = {
    print("MP1 is ending; enter BP? (If not, will go to EP) ")
    StdIn.readBoolean()
  }

  /**
    * Ask the user for a specific element of a sequence.
    */
  private def select[A](prompt: String, options: Seq[A]): A = {
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

  /**
    * After SP, will ask via StdIn, otherwise just consents.
    */
  override def consentToEnd(implicit gameState: GameState, turnPlayer: Player, phase: Phase, step: Step): Boolean = {
    phase match {
      case MainPhase | BattlePhase | MainPhase2 | EndPhase =>
        print(s"End ${Option(step).getOrElse(phase)}? ")
        StdIn.readBoolean()
      case _ =>
        true
    }
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
  override def cardToDiscardForHandSizeLimit(implicit gameState: GameState, turnPlayer: Player, phase: Phase, step: Step): Card = hand.head
  override def chooseAction(actions: Seq[Action])(implicit gameState: GameState, turnPlayer: Player, phase: Phase, step: Step ) = actions.head
  override def consentToEnd(implicit gameState: GameState, turnPlayer: Player, phase: Phase, step: Step): Boolean = true
  override def enterBattlePhase(implicit gameState: GameState, turnPlayer: Player, phase: Phase, step: Step = null): Boolean = false
}
