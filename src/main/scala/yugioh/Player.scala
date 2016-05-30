package yugioh

import yugioh.action.Action
import yugioh.card.Card
import yugioh.card.monster.{ExtraDeckMonster, Monster}
import yugioh.events.Observable.observe
import yugioh.events.{PhaseStartEvent, TurnStartEvent}

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
  def chooseAction(actions: Seq[Action])(implicit gameState: GameState, turnPlayers: TurnPlayers, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step = null): Action

  /**
    * Called when MP1 is ended.
    *
    * @return true to enter BP, otherwise go to EP
    */
  def enterBattlePhase(implicit gameState: GameState, turnPlayers: TurnPlayers, phase: Phase, step: Step = null): Boolean

  def draw(): Unit = draw(1)
  def draw(howMany: Int): Unit = {
    val toAdd = deck.fromTop(howMany)
    for (card <- toAdd) {
      card.location = InHand
    }

    hand ++= toAdd
  }

  def cardToDiscardForHandSizeLimit(implicit gameState: GameState, turnPlayers: TurnPlayers, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step = null): Card

  /**
    * Ask player if they wish to end the phase or step.
    */
  def consentToEnd(implicit gameState: GameState, turnPlayers: TurnPlayers, phase: Phase, step: Step = null): Boolean

  /**
    * Ask a player what summon material (tribute, synchro, etc.) they wish to use for toSummon.
    */
  def selectSummonMaterial(toSummon: Monster, possibleMaterials: Seq[Monster]): Seq[Monster]

  override def toString = name
}

case class TurnPlayers(turnPlayer: Player, opponent: Player)

class CommandLineHumanPlayer(val name: String) extends Player {
  override val deck: Deck = new TestDeck(this) // TODO: be more than just a stub

  observe { event =>
    event match {
      case TurnStartEvent(turnPlayers, gameState) =>
        println(s"Turn number ${gameState.turnCount}, turn player ${turnPlayers.turnPlayer}.")
      case phaseStart: PhaseStartEvent =>
        println(s"Entering ${phaseStart.phase}")
      case ignore =>
    }
  }

  override def chooseAction(actions: Seq[Action])(implicit gameState: GameState, turnPlayers: TurnPlayers, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step) = {
    if (actions.size == 1) {
      val action = actions.head
      println(s"Action $action was only option ($fastEffectTiming, $phase${Option(step).map(", " + _).getOrElse("")}), taking implicitly.")
      action
    } else {
      select("Select an action:", actions)
    }
  }

  override def cardToDiscardForHandSizeLimit(implicit gameState: GameState, turnPlayers: TurnPlayers, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step): Card = {
    select("Select a card to discard:", hand)
  }

  override def enterBattlePhase(implicit gameState: GameState, turnPlayers: TurnPlayers, phase: Phase, step: Step) = {
    print("MP1 is ending; enter BP? (If not, will go to EP) ")
    StdIn.readBoolean()
  }

  /**
    * Ask the user for a specific element of a sequence.
    */
  private def select[A](prompt: String, options: Seq[A])
                       (implicit gameState: GameState, turnPlayers: TurnPlayers, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step): A = {
    println(prompt + s" ($fastEffectTiming, $phase${Option(step).map(", " + _).getOrElse("")})")

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
  override def consentToEnd(implicit gameState: GameState, turnPlayers: TurnPlayers, phase: Phase, step: Step): Boolean = {
    phase match {
      case MainPhase | BattlePhase | MainPhase2 | EndPhase if turnPlayers.turnPlayer == this =>
        print(s"End ${Option(step).getOrElse(phase)}? ")
        StdIn.readBoolean()
      case _ =>
        true
    }
  }

  // TODO: selecting summon material needs to enforce further constraints, e.g. level 5 cannot tribute 2 monsters, 7 requires 2 not just 1
  override def selectSummonMaterial(toSummon: Monster, possibleMaterials: Seq[Monster]) = {
    println(s"To summon $toSummon, please enter comma separated monster(s) to use.")

    for ((monster, i) <- possibleMaterials.zipWithIndex) {
      println(s"($i) $monster")
    }

    val choices = StdIn.readLine("> ").split(",").map(_.toInt)
    choices.map(possibleMaterials(_))
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
  override def cardToDiscardForHandSizeLimit(implicit gameState: GameState, turnPlayers: TurnPlayers, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step): Card = hand.head
  override def chooseAction(actions: Seq[Action])(implicit gameState: GameState, turnPlayers: TurnPlayers, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step ) = actions.head
  override def consentToEnd(implicit gameState: GameState, turnPlayers: TurnPlayers, phase: Phase, step: Step): Boolean = true
  override def enterBattlePhase(implicit gameState: GameState, turnPlayers: TurnPlayers, phase: Phase, step: Step = null): Boolean = false
  override def selectSummonMaterial(toSummon: Monster, possibleMaterials: Seq[Monster]) = ???
}
