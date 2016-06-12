package yugioh.action

import yugioh._
import yugioh.events.{DefaultEventsComponent, Event, EventsComponent}

trait Action extends Event {
  self: EventsComponent =>

  private var previouslyCalled = false

  def execute()(implicit gameState: GameState): Action = {
    if (previouslyCalled) {
      throw new IllegalStateException("Action " + this + " was already executed.")
    }

    doAction()
    events.emit(this)
    gameState.history.append(this)
    previouslyCalled = true

    this
  }

  def undo()(implicit gameState: GameState): Unit = throw new NotImplementedError("Undo has not been implemented for " + this)

  def redo()(implicit gameState: GameState): Unit = throw new NotImplementedError("Redo has not been implemented for " + this)

  protected def doAction()(implicit gameState: GameState): Unit
}

trait InherentAction extends Action with DefaultEventsComponent

/**
  * Composition of InherentAction(s).
  *
  * TODO: should also have activation condition, cost, and effect.
  */
trait Activation extends Action with DefaultEventsComponent

trait PassPriority extends InherentAction {
  override def toString = "PassPriority"
}

class PassPriorityImpl extends PassPriority {
  override def doAction()(implicit gameState: GameState) = ()
}

trait Set extends InherentAction

trait Discard extends InherentAction

class DiscardImpl extends Discard {
  override protected def doAction()(implicit gameState: GameState) = {
    val player = gameState.turnPlayers.turnPlayer
    val choice = player.cardToDiscardForHandSizeLimit
    val card = player.hand.remove(player.hand.indexOf(choice))
    card.owner.field.graveyard.append(card)
  }
}

trait Draw extends InherentAction

trait DrawForTurn extends Draw

class DrawForTurnImpl extends DrawForTurn {
  override protected def doAction()(implicit gameState: GameState): Unit = {
    gameState.turnPlayers.turnPlayer.draw()
  }
}

