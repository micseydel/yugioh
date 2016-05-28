package yugioh.action

import yugioh._

trait Action {
  private var called = false

  def execute()(implicit gameState: GameState, turnPlayers: TurnPlayers, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step = null): Unit = {
    if (called) {
      throw new IllegalStateException("Action " + this + " was already executed.")
    }

    doAction()
    // TODO: will want emit an event here
    gameState.history :+= this
    called = true
  }

  def undo()(implicit gameState: GameState): Unit = throw new NotImplementedError("Undo has not been implemented for " + this)

  def redo()(implicit gameState: GameState): Unit = throw new NotImplementedError("Redo has not been implemented for " + this)

  protected def doAction()(implicit gameState: GameState, turnPlayers: TurnPlayers, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step): Unit
}

trait InherentAction extends Action

/**
  * Composition of InherentAction(s).
  */
trait Activation extends Action

trait PassPriority extends InherentAction {
  override def toString = "PassPriority"
}

class PassPriorityImpl extends PassPriority {
  override def doAction()(implicit gameState: GameState, turnPlayers: TurnPlayers, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step) = ()
}

trait Set extends InherentAction

trait Discard extends InherentAction

class DiscardImpl extends Discard {
  override protected def doAction()(implicit gameState: GameState, turnPlayers: TurnPlayers, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step) = {
    val player = turnPlayers.turnPlayer
    val choice = player.cardToDiscardForHandSizeLimit
    val card = player.hand.remove(player.hand.indexOf(choice))
    card.owner.field.graveyard.append(card)
  }
}

trait Draw extends InherentAction

trait DrawForTurn extends Draw

class DrawForTurnImpl extends DrawForTurn {
  override protected def doAction()(implicit gameState: GameState, turnPlayers: TurnPlayers, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step): Unit = {
    turnPlayers.turnPlayer.draw()
  }
}

