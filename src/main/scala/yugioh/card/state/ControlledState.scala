package yugioh.card.state

import yugioh.card.monster.Position

trait ControlledState {
  def faceup: Boolean
}

trait MonsterControlledState extends ControlledState {
  var position: Position

  // TODO: these need listeners for the turn ending
  var attackedThisTurn = false
  var manuallyChangedPositionsThisTurn = false // also set to true if attacked

  override def faceup: Boolean = Position.FaceUp.contains(position)
}

class MonsterControlledStateImpl(override var position: Position) extends MonsterControlledState
