package yugioh.card

import yugioh.{GameState, Location, Player}
import yugioh.card.state.{FieldState, HandState}
import yugioh.action.Action

trait Card {
  val printedName: String
  val owner: Player

  def handState: Option[HandState]
  def fieldState: Option[FieldState]

  def controller: Player

  def location: Location

  def actions(implicit gameState: GameState): Seq[Action]

  // TODO these probably need references to the field?
  def sendToGrave()(implicit gameState: GameState): Unit
  def destroy()(implicit gameState: GameState): Unit
  def banish()(implicit gameState: GameState): Unit
  def toHand()(implicit gameState: GameState): Unit
  def toDeck()(implicit gameState: GameState): Unit

  def name: String = printedName
}

trait SpellOrTrap extends Card

trait Delegate

/**
  * This is a wrapper so that a Monster can be treated as a SpellOrTrap card.
  */
trait SpellOrTrapDelegate extends Delegate with SpellOrTrap
