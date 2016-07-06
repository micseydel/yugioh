package yugioh.card.state

import yugioh.card.monster.Position
import yugioh.events.{DefaultEventsModuleComponent, TurnEndEvent}

trait ControlledState {
  def faceup: Boolean
}

case class SpellTrapControlledState(var faceup: Boolean) extends ControlledState

case class MonsterControlledState(
  var position: Position,
  var attackedThisTurn: Boolean = false,
  var manuallyChangedPositionsThisTurn: Boolean = false // also set to true if attacked
) extends ControlledState with DefaultEventsModuleComponent {
  // TODO: need clean way of unsubscribing
  val subscription = eventsModule.observe { event =>
    event match {
      case TurnEndEvent =>
        attackedThisTurn = false
        manuallyChangedPositionsThisTurn = false
      case ignore =>
    }
  }

  override def faceup: Boolean = Position.FaceUp.contains(position)
}
