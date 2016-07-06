package yugioh.card

import yugioh.action.ExistsInAChainAction
import yugioh.{Criteria, GameState}


/**
  * An effect will go on chain, and its activation will occur, and its resolution will occur as the chain resolves.
  */
trait Effect {
  val Card: Card
  val Conditions: Conditions
  val Activation: Activation
  val Resolution: Resolution

  protected[this] var selectedTargets: Seq[Card] = null

  /**
    * Helper method for determining if an effect targets or not.
    */
  final def doesTarget(implicit gameState: GameState) = targetCriteria != null

  /**
    * Criteria to determine if target(s) are valid.
    */
  def targetCriteria[C <: Card](implicit gameState: GameState): Criteria[C] = null

  /**
    * Available targets to choose from, based on the targetCriteria.
    */
  def availableTargets(implicit gameState: GameState): Seq[Card] = null
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
  /**
    * The effect to which this resolution belongs.
    */
  val Effect: Effect

  def resolve(existsInAChainAction: ExistsInAChainAction)(implicit gameState: GameState): Unit
}

trait FlipEffect extends Effect {

}
