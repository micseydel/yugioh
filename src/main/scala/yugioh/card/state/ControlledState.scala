package yugioh.card.state

import yugioh.card.monster.Position
import yugioh.events.Observable.observe
import yugioh.events.TurnEndEvent

trait ControlledState {
  def faceup: Boolean
}

case class MonsterControlledState(
  var position: Position,
  var attackedThisTurn: Boolean = false,
  var manuallyChangedPositionsThisTurn: Boolean = false // also set to true if attacked
) extends ControlledState {
  // TODO: need clean way of unsubscribing
  val subscription = observe { event =>
    event match {
      case TurnEndEvent =>
        attackedThisTurn = false
        manuallyChangedPositionsThisTurn = false
      case ignore =>
    }
  }

  override def faceup: Boolean = Position.FaceUp.contains(position)
}
