package yugioh.action.card

import yugioh.action.{ActionModule, Cause, InherentAction}
import yugioh.card.{Card, SpellOrTrap}
import yugioh.card.monster.Monster
import yugioh.card.state.ControlledState
import yugioh.events.EventsModule
import yugioh.{GameState, Location}

case class CardMovedAction(cause: Cause, card: Card[_ <: ControlledState], from: Location, to: Location) extends InherentAction {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    // if the card moved to a non-field zone, reset its field-specific state
    if (!Location.FieldZones.contains(to)) {
     card.maybeControlledState = None

      card match {
        case monster: Monster =>
          monster.maybeMonsterFieldState = None
        case spellOrTrap: SpellOrTrap =>
          // TODO: there will be S/T state eventually
      }
    }

    card.location = to
  }
}
