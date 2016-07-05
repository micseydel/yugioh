package yugioh.action.monster

import yugioh._
import yugioh.action.InherentAction
import yugioh.card.monster.{Attack, Defense, Monster, Set}

trait SwitchPosition extends InherentAction {
  val monster: Monster
  override def toString = s"${this.getClass.getSimpleName}($monster(${monster.maybeMonsterControlledState.get.position}))"

  override protected def doAction()(implicit gameState: GameState) = {
    for (controlledState <- monster.maybeMonsterControlledState) {
      controlledState.manuallyChangedPositionsThisTurn = true
      controlledState.position = controlledState.position match {
        case Attack => Defense
        case Defense | Set => Attack
        case _ => throw new IllegalStateException("Shouldn't have tried to switch monster position.")
      }
    }
  }
}

class SwitchPositionImpl(override val monster: Monster) extends SwitchPosition {
  val player = monster.owner
}
