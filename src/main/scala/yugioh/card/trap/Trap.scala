package yugioh.card.trap

import yugioh._
import yugioh.action.{Action, ActionModule, CardActivation, InherentAction}
import yugioh.card.state.SpellTrapControlledState
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
  override lazy val StateChange = new InherentAction {
    override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
      // TODO HIGH: this should not be part of the trap effect, but rather, part of card activation
      Card.location match {
        case InHand =>
          Card.controller.field.placeAsSpellOrTrap(Card.asInstanceOf[SpellOrTrap], faceup = true)
        case _: InSpellTrapZone =>
          Card.maybeControlledState.get.asInstanceOf[SpellTrapControlledState].faceup = true
        case _ =>
          throw new IllegalStateException("A regular trap should not be activated from anywhere except set Spell/Trap zones.")
      }
    }

    override lazy val player: Player = Card.controller
  }
}
