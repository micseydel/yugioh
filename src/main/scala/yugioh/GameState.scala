package yugioh

import yugioh.card.monster.Monster


case class GameState(mutableGameState: MutableGameState,
                     turnPlayers: TurnPlayers,
                     fastEffectTiming: FastEffectTiming = null,
                     phase: Phase = null,
                     step: Step = null,
                     window: Window = null) {
  def turnCount = mutableGameState.turnCount
  def hasNormalSummonedThisTurn = mutableGameState.hasNormalSummonedThisTurn
  def history = mutableGameState.history
}

/**
  * A response window, which allows one chain before closing.
  */
sealed trait Window

// TODO: some way of keeping track of what kind of summon, perhaps?
case class SummonWindow(monster: Monster) extends Window

/**
  * Empty target indicates a direct attack.
  */
case class AttackWindow(attacker: Monster, maybeTarget: Option[Monster] = None) extends Window
