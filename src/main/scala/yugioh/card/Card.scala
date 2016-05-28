package yugioh.card

import yugioh._
import yugioh.action.Action
import yugioh.card.monster.Monster
import yugioh.card.state.ControlledState

trait Card {
  val printedName: String
  val owner: Player

  var location: Location = InDeck

  var maybeControlledState: Option[ControlledState] = None

  def controller: Player = owner

  def actions(implicit gameState: GameState, turnPlayers: TurnPlayers, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step = null): Seq[Action]

  def name: String = printedName
}

trait SpellOrTrap extends Card

trait Delegate

/**
  * This is a wrapper so that a Monster can be treated as a SpellOrTrap card.
  */
trait SpellOrTrapDelegate extends Delegate with SpellOrTrap

/**
  * This is essentially a wrapper that allows a SpellOrTrap to be treated as a Monster card.
  */
trait MonsterDelegate extends Delegate with Monster
