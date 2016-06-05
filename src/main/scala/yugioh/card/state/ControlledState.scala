package yugioh.card.state

import yugioh.card.monster.Position
import yugioh.events.Observable.observe
import yugioh.events.TurnEndEvent

trait ControlledState {
  def faceup: Boolean
}

trait MonsterControlledState extends ControlledState {
  var position: Position

  var attackedThisTurn = false
  var manuallyChangedPositionsThisTurn = false // also set to true if attacked

  override def faceup: Boolean = Position.FaceUp.contains(position)
}

class MonsterControlledStateImpl(override var position: Position) extends MonsterControlledState {
  val subscription = observe { event =>
    event match {
      case TurnEndEvent =>
        attackedThisTurn = false
        manuallyChangedPositionsThisTurn = false
      case ignore =>
    }
  }
}
