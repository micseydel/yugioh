package yugioh.card.spell

import sun.reflect.generics.reflectiveObjects.NotImplementedException
import yugioh._
import yugioh.action.{Action, ActionModule, CardActivation, InherentAction}
import yugioh.card._
import yugioh.card.state.SpellTrapControlledState
import yugioh.events.EventsModule

sealed trait Spell extends SpellOrTrap {
  override def actions(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Seq[Action] = {
    val maybeActivation = if (Effects.size == 1) {
      gameState match {
        case GameState(_, TurnPlayers(Owner, _), OpenGameState, MainPhase | MainPhase2, _, _) if canActivate =>
          Seq(CardActivation(this, Owner))
        case _ => Seq()
      }
    } else {
      throw new NotImplementedException()
    }

    // super allows setting
    super.actions ++ maybeActivation
  }

  private def canActivate(implicit gameState: GameState): Boolean = {
    // also assumes a single effect
    // if activation condition is met, and the spell is either already on the field or it's in hand and there's space to place it...
    Effects.head.ActivationConditions.met && (InSpellTrapZone(this) || (InHand(this) && controller.field.hasFreeSpellOrTrapZone))
  }
}

trait NormalSpell extends Spell with NormalSpellOrTrap
trait EquipSpell extends Spell
trait QuickPlaySpell extends Spell
trait RitualSpell extends Spell
trait ContinuousSpell extends Spell with ContinuousSpellOrTrap

trait SpellEffect extends Effect {
  override def activationTimingCorrect(implicit gameState: GameState): Boolean = {
    gameState match {
      case GameState(_, TurnPlayers(Card.Owner, _), OpenGameState, MainPhase | MainPhase2, _, _) => true
      case _ => false
    }
  }

  // TODO: this should not be part of the spell effect, but rather, part of spell card activation
  override lazy val StateChange = new InherentAction {
    override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
      Card.location match {
        case InHand =>
          Card.controller.field.placeAsSpellOrTrap(Card.asInstanceOf[SpellOrTrap], faceup = true)
        case _: InSpellTrapZone =>
          Card.maybeControlledState.get.asInstanceOf[SpellTrapControlledState].faceup = true
        case _ =>
          throw new IllegalStateException("A regular spell should not be activated from anywhere except the hand or set Spell/Trap zones.")
      }
    }

    override lazy val player: Player = Card.controller
  }
}

trait FieldSpell extends Spell
