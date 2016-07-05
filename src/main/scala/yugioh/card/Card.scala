package yugioh.card

import yugioh._
import yugioh.action.{Action, SetCard}
import yugioh.card.monster.Monster
import yugioh.card.state.ControlledState

trait Card {
  val PrintedName: String
  val Owner: Player
  var location: Location = InDeck

  var controller: Player = Owner
  var maybeControlledState: Option[ControlledState] = None
  def actions(implicit gameState: GameState): Seq[Action]

  def name: String = PrintedName

  override def toString = name

  def toString(viewer: Player): String = {
    maybeControlledState.map { controlledState =>
      if (controlledState.faceup) {
        name
      } else {
        if (viewer == Owner) {
          s"Set($this)"
        } else {
          "<Set>"
        }
      }
    }.getOrElse(name)
  }

  def destroy() = Owner.field.destroy(this)
  def sendToGrave() = Owner.field.sendToGrave(this)
}

trait SpellOrTrap extends Card {
  val effects: List[Effect]

  override def actions(implicit gameState: GameState): Seq[Action] = {
    gameState match {
      case GameState(_, TurnPlayers(Owner, _), OpenGameState, MainPhase | MainPhase2, _, _) if InHand(this) =>
        Seq(new SetAsSpellOrTrapImpl(this)) // TODO: decouple
      case _ => Seq()
    }
  }
}

trait SetAsSpellOrTrap extends SetCard {
  val spellOrTrap: SpellOrTrap
}

class SetAsSpellOrTrapImpl(override val spellOrTrap: SpellOrTrap) extends SetAsSpellOrTrap {
  override val player: Player = spellOrTrap.Owner

  override val toString = s"SetAsSpellOrTrap($spellOrTrap)"

  override protected def doAction()(implicit gameState: GameState): Unit = {
    if (player.field.hasFreeSpellOrTrapZone) {
      player.field.placeAsSpellOrTrap(spellOrTrap, faceup = false)
    } else {
      throw new IllegalStateException(s"Tried to set $spellOrTrap but there was no space.")
    }
  }
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
