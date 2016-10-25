package yugioh.card.trap

import yugioh.action.{ActionModule, InherentAction}
import yugioh.card.state.SpellTrapControlledState
import yugioh._
import yugioh.card.{Effect, NonContinuousSpellOrTrap, SpellOrTrap}
import yugioh.events.EventsModule

sealed trait Trap extends SpellOrTrap

trait NormalTrap extends Trap with NonContinuousSpellOrTrap
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
