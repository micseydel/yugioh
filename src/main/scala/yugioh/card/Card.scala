package yugioh.card

import yugioh._
import yugioh.card.state.{FieldState, HandState}
import yugioh.action.Action

trait Card {
  val printedName: String
  val owner: Player

  def handState: Option[HandState] = None
  def fieldState: Option[FieldState] = None

  def controller: Player = owner

  def actions(implicit gameState: GameState, turnPlayer: Player, phase: Phase, maybeStep: Option[Step]): Seq[Action]

  def name: String = printedName
  def location: Location = InDeck
}

trait SpellOrTrap extends Card

trait Delegate

/**
  * This is a wrapper so that a Monster can be treated as a SpellOrTrap card.
  */
trait SpellOrTrapDelegate extends Delegate with SpellOrTrap
