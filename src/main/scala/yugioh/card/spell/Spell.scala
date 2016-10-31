package yugioh.card.spell

import sun.reflect.generics.reflectiveObjects.NotImplementedException
import yugioh._
import yugioh.action.{Action, ActionModule, CardActivation}
import yugioh.card._
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
    assert(Effects.length == 1, "Code assumes only a single effect will be present.")

    // if activation condition is met, and the spell is either already on the field or it's in hand and there's space to place it...
    Effects.head.ActivationConditions.met && (InSpellTrapZone(this) || (InHand(this) && controller.field.hasFreeSpellOrTrapZone))
  }
}

trait NormalSpell extends Spell with NonContinuousSpellOrTrap
trait QuickPlaySpell extends Spell with NonContinuousSpellOrTrap
trait RitualSpell extends Spell with NonContinuousSpellOrTrap
trait EquipSpell extends Spell
trait ContinuousSpell extends Spell
trait FieldSpell extends Spell

trait SpellEffect extends Effect {
  override def activationTimingCorrect(implicit gameState: GameState): Boolean = {
    gameState match {
      case GameState(_, TurnPlayers(Card.Owner, _), OpenGameState, MainPhase | MainPhase2, _, _) => true
      case _ => false
    }
  }
}
