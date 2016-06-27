package yugioh

import yugioh.events.Event


case class GameState(mutableGameState: MutableGameState,
                     turnPlayers: TurnPlayers,
                     fastEffectTiming: FastEffectTiming = null,
                     phase: Phase = null,
                     step: Step = null,
                     inResponseTo: List[Event] = Nil) {
  def turnCount = mutableGameState.turnCount
  def hasNormalSummonedThisTurn = mutableGameState.hasNormalSummonedThisTurn
  def history = mutableGameState.history
}
