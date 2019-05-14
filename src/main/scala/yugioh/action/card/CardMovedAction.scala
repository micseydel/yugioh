package yugioh.action.card

import yugioh.action.{ActionModule, Cause, InherentAction}
import yugioh.card.Card
import yugioh.card.state.ControlledState
import yugioh.events.EventsModule
import yugioh.{GameState, Location}

case class CardMovedAction(cause: Cause, card: Card[_ <: ControlledState], from: Location, to: Location) extends InherentAction {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    card.updateLocation(to)
  }
}
