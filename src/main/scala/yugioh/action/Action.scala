package yugioh.action

import yugioh.card.monster.Monster
import yugioh._

trait Action {
  private var called = false

  def execute()(implicit gameState: GameState, player: Player, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step = null): Unit = {
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

  protected def doAction()(implicit gameState: GameState, player: Player, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step): Unit
}

trait InherentAction extends Action

trait Summon extends InherentAction {
  def monster: Monster

  override def toString = s"${this.getClass.getSimpleName}(${monster.getClass.getSimpleName})"
}

trait NormalSummon extends Summon

trait SetAsMonster extends InherentAction

trait TributeSetAsMonster extends SetAsMonster

class NormalSummonImpl(val monster: Monster) extends NormalSummon {
  override protected def doAction()(implicit gameState: GameState, player: Player, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step) = {
    player.field.placeAsMonster(monster)
    gameState.hasNormalSummonedThisTurn = true // TODO: change this in an event-based system
  }
}

trait TributeSummon extends NormalSummon

// TODO BUG: was offered during TurnPlayerFastEffects and resulted in a match exception
class TributeSummonImpl(override val monster: Monster) extends NormalSummonImpl(monster) with TributeSummon {
  override protected def doAction()(implicit gameState: GameState, player: Player, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step) = {
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
  override def doAction()(implicit gameState: GameState, player: Player, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step) = ()
}

trait Set extends InherentAction

trait Discard extends InherentAction

class DiscardImpl extends Discard {
  override protected def doAction()(implicit gameState: GameState, player: Player, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step) = {
    val choice = player.cardToDiscardForHandSizeLimit
    val card = player.hand.remove(player.hand.indexOf(choice))
    card.owner.field.graveyard.append(card)
  }
}

trait Draw extends InherentAction

trait DrawForTurn extends Draw

class DrawForTurnImpl extends DrawForTurn {
  override protected def doAction()(implicit gameState: GameState, player: Player, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step): Unit = {
    player.draw()
  }
}

