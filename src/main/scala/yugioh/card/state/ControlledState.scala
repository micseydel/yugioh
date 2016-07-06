package yugioh.card.state

import yugioh.card.monster.Position
import yugioh.events.{EventsModule, TurnEndEvent}

trait ControlledState {
  def faceup: Boolean
}

case class SpellTrapControlledState(var faceup: Boolean) extends ControlledState

case class MonsterControlledState(
  var position: Position,
  var attackedThisTurn: Boolean = false,
  var manuallyChangedPositionsThisTurn: Boolean = false // also set to true if attacked
)(implicit eventsModule: EventsModule) extends ControlledState {
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
