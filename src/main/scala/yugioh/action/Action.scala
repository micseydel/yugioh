package yugioh.action

import yugioh.{GameState, Player}

trait Action {
  private var called = false

  def execute()(implicit gameState: GameState, player: Player): Unit = {
    if (called) {
      throw new IllegalStateException("Action " + this + " was already executed.")
    }

    doAction()
    // TODO probably want to add to game state history here, emit an event
    called = true
  }

  def undo()(implicit gameState: GameState): Unit = throw new NotImplementedError("Undo has not been implemented for " + this)

  def redo()(implicit gameState: GameState): Unit = throw new NotImplementedError("Redo has not been implemented for " + this)

  protected def doAction()(implicit gameState: GameState, player: Player): Unit
}

trait InherentAction extends Action

/**
  * Composition of InherentAction(s).
  */
trait Activation extends Action

trait PassPriority extends InherentAction

class PassPriorityImpl extends PassPriority {
  override def doAction()(implicit gameState: GameState, player: Player) = ()
}

trait Set extends InherentAction

trait Discard extends InherentAction

class DiscardImpl extends Discard {
  override protected def doAction()(implicit gameState: GameState, player: Player) = {
    val choice = player.cardToDiscardForHandSizeLimit
    val card = player.hand.remove(player.hand.indexOf(choice))
    card.owner.field.Graveyard.append(card)
  }
}
