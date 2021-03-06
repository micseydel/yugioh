package yugioh.action.monster

import yugioh._
import yugioh.action.{ActionModule, Cause, InherentAction}
import yugioh.card.monster.{Attack, Defense, Monster, Set}
import yugioh.events.EventsModule

trait SwitchPosition extends InherentAction {
  protected[this] implicit val eventsModule: EventsModule

  val monster: Monster
  override def toString = s"${this.getClass.getSimpleName}($monster(${monster.maybeControlledState.get.position}))"

  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    for (controlledState <- monster.maybeControlledState) {
      controlledState.manuallyChangedPositionsThisTurn = true
      controlledState.position = controlledState.position match {
        case Attack => Defense
        case Defense | Set => Attack
        case _ => throw new IllegalStateException("Shouldn't have tried to switch monster position.")
      }
    }
  }
}

class SwitchPositionImpl(override val cause: Cause, override val monster: Monster)(implicit override val eventsModule: EventsModule)
    extends SwitchPosition {
  val player: Player = monster.Owner
}
