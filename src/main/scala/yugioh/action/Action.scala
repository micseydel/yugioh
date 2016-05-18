package yugioh.action

import yugioh.{GameState, Player}

// TODO probably need implicit player
trait Action {
  def doAction()(implicit gameState: GameState, player: Player)
  def undoAction()(implicit gameState: GameState)
}

// TODO: pass priority

trait InherentAction extends Action
trait Activation extends Action

trait Set extends InherentAction
