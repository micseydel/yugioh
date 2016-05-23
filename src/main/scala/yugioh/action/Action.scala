package yugioh.action

import yugioh.card.monster.Monster
import yugioh.{GameState, Phase, Player, Step}

trait Action {
  private var called = false

  def execute()(implicit gameState: GameState, player: Player, phase: Phase, step: Step = null): Unit = {
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

  protected def doAction()(implicit gameState: GameState, player: Player, phase: Phase, step: Step): Unit
}

trait InherentAction extends Action

trait Summon extends InherentAction

trait NormalSummon extends Summon

class NormalSummonImpl(val monster: Monster) extends NormalSummon {
  override protected def doAction()(implicit gameState: GameState, player: Player, phase: Phase, step: Step) = player.field.placeAsMonster(monster)
}

trait TributeSummon extends NormalSummon

class TributeSummonImpl(override val monster: Monster) extends NormalSummonImpl(monster) with TributeSummon {
  override protected def doAction()(implicit gameState: GameState, player: Player, phase: Phase, step: Step) = {
    // TODO: tribute
    super.doAction()
  }
}

/**
  * Composition of InherentAction(s).
  */
trait Activation extends Action

trait PassPriority extends InherentAction {
  override def toString = "PassPriority"
}

class PassPriorityImpl extends PassPriority {
  override def doAction()(implicit gameState: GameState, player: Player, phase: Phase, step: Step) = ()
}

trait Set extends InherentAction

trait Discard extends InherentAction

class DiscardImpl extends Discard {
  override protected def doAction()(implicit gameState: GameState, player: Player, phase: Phase, step: Step) = {
    val choice = player.cardToDiscardForHandSizeLimit
    val card = player.hand.remove(player.hand.indexOf(choice))
    card.owner.field.Graveyard.append(card)
  }
}

trait Draw extends InherentAction

trait DrawForTurn extends Draw

class DrawForTurnImpl extends DrawForTurn {
  override protected def doAction()(implicit gameState: GameState, player: Player, phase: Phase, step: Step): Unit = {
    player.draw()
  }
}

