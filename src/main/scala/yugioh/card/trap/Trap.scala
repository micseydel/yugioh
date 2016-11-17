package yugioh.card.trap

import yugioh._
import yugioh.action.{Action, ActionModule, CardActivation}
import yugioh.card.{Effect, NonContinuousSpellOrTrap, SpellOrTrap}
import yugioh.events.EventsModule

sealed trait Trap extends SpellOrTrap

trait NormalTrap extends Trap with NonContinuousSpellOrTrap {
  override def actions(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Seq[Action] = {
    val maybeActivation = Effects match {
      case Seq(effect) if canActivate && effect.activationTimingCorrect =>
        Seq(CardActivation(this, Owner))
      case _ =>
        Seq() // subclass will have to write specific logic for this
    }

    // super handles setting
    super.actions ++ maybeActivation
  }

  private def canActivate(implicit gameState: GameState): Boolean = {
    // if activation condition is met, and the spell is already set on the field, and it wasn't set this turn...
    Effects.head.ActivationConditions.met && InSpellTrapZone(this) && maybeControlledState.map(!_.faceup).get && maybeTurnSet.exists(_ < gameState.turnCount)
  }
}

trait CounterTrap extends Trap with NonContinuousSpellOrTrap
trait ContinuousTrap extends Trap

trait TrapEffect extends Effect {
  /**
    * Applicable for non-counter traps.
    */
  override def activationTimingCorrect(implicit gameState: GameState): Boolean = {
    gameState match {
      case GameState(_, _, _, _, _: DamageStep, _) =>
        false
      case GameState(_, _, _: CheckForTrigger | _: PlayerFastEffects | _: ChainRules, _, _, _) =>
        true
      case _ =>
        false
    }
  }
}