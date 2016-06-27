package yugioh.card

import yugioh._
import yugioh.action.Action
import yugioh.card.monster.Monster
import yugioh.card.state.ControlledState

trait Card {
  val PrintedName: String
  val owner: Player
  var location: Location = InDeck

  var controller: Player = owner
  var maybeControlledState: Option[ControlledState] = None
  def actions(implicit gameState: GameState): Seq[Action]

  def name: String = PrintedName

  override def toString = name

  def toString(viewer: Player): String = {
    maybeControlledState.map { controlledState =>
      if (controlledState.faceup) {
        name
      } else {
        if (viewer == owner) {
          s"Set($this)"
        } else {
          "<Set>"
        }
      }
    }.getOrElse(name)
  }

  def sendToGrave() = owner.field.sendToGrave(this)
}

trait SpellOrTrap extends Card {
  val effects: List[Effect]
}

sealed trait Delegate

/**
  * This is a wrapper so that a Monster can be treated as a SpellOrTrap card.
  */
trait SpellOrTrapDelegate extends Delegate with SpellOrTrap

/**
  * This is essentially a wrapper that allows a SpellOrTrap to be treated as a Monster card.
  */
trait MonsterDelegate extends Delegate with Monster
