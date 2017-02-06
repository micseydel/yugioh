package yugioh

import yugioh.events.Event


/**
  * @param mutableGameState state which changes as the game progresses
  * @param turnPlayers null before the first turn begins
  * @param fastEffectTiming state in the fast effect tiing state machine
  * @param phase phase in the phase state machine
  * @param step optional, step in battle phase
  * @param inResponseTo the "last thing(s) to happen" which can be responded to at this time
  */
case class GameState(mutableGameState: MutableGameState,
                     turnPlayers: TurnPlayers,
                     fastEffectTiming: FastEffectTiming = null,
                     phase: Phase = null,
                     step: Step = null,
                     inResponseTo: List[Event] = Nil) {
  def turnCount: Int = mutableGameState.turnCount
  def hasNormalSummonedThisTurn: Boolean = mutableGameState.hasNormalSummonedThisTurn
  // TODO LOW: history
}
