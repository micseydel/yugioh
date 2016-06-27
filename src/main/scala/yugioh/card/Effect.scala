package yugioh.card

import yugioh.GameState


/**
  * An effect will go on chain, and its activation will occur, and its resolution will occur as the chain resolves.
  */
trait Effect {
  val Card: Card
  val Conditions: Conditions
  val Activation: Activation
  val Resolution: Resolution
}

/**
  * Any condition(s) for activation; this includes valid targets and fast effect timing.
  */
trait Conditions {
  def met(implicit gameState: GameState): Boolean
}

/**
  * This includes cost and targeting.
  */
trait Activation {
  def activate()(implicit gameState: GameState): Unit
}

/**
  * Composed of inherent actions.
  *
  * This encapsulates the complexity of "if then" and "and then" and such.
  */
trait Resolution {
  def resolve()(implicit gameState: GameState): Unit
}
