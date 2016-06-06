package yugioh

import yugioh.action.DrawForTurnImpl
import yugioh.events.Observable.emit
import yugioh.events._

/**
  * State machine.
  */
sealed trait Phase {
  val abbreviation: String

  def next(gameState: GameState): Phase
}


case object DrawPhase extends Phase {
  val abbreviation = "DP"

  override def next(gameState: GameState): Phase = {
    implicit val newGameState = gameState.copy(phase = this)

    if (gameState.mutableGameState.turnCount > 1) {
      val drawForTurn = new DrawForTurnImpl
      drawForTurn.execute()
      FastEffectTiming.loop(newGameState, start = CheckForTrigger(drawForTurn))
    } else {
      FastEffectTiming.loop(newGameState)
    }

    StandbyPhase
  }
}

case object StandbyPhase extends Phase {
  val abbreviation = "SP"

  override def next(gameState: GameState): Phase = {
    FastEffectTiming.loop(gameState.copy(phase = this))
    MainPhase
  }
}

case object MainPhase extends Phase {
  val abbreviation = "MP"

  /**
    * If it's later than the first turn, ask the turn player if they want to go to BP. Otherwise go to EP.
    */
  override def next(gameState: GameState): Phase = {
    implicit val newGameState = gameState.copy(phase = this)

    FastEffectTiming.loop(newGameState)
    if (gameState.turnCount > 1 && gameState.turnPlayers.turnPlayer.enterBattlePhase) {
      BattlePhase
    } else {
      EndPhase
    }
  }
}

case object BattlePhase extends Phase {
  val abbreviation = "BP"

  override def next(gameState: GameState): Phase = {
    BattlePhaseStep.loop(gameState.copy(phase = this))
    MainPhase2
  }
}

case object MainPhase2 extends Phase {
  val abbreviation = "MP2"

  override def next(gameState: GameState): Phase = {
    FastEffectTiming.loop(gameState.copy(phase = this))
    EndPhase
  }
}

case object EndPhase extends Phase {
  val abbreviation = "EP"

  override def next(gameState: GameState): Phase = {
    FastEffectTiming.loop(gameState.copy(phase = this))
    null
  }
}


object Phase {
  val phases = Seq(DrawPhase, StandbyPhase, MainPhase, BattlePhase, MainPhase2, EndPhase)
  val mainPhases = Set(MainPhase, MainPhase2)

  private val StartEvents: Map[Phase, PhaseStartEvent] = Map(
    DrawPhase -> DrawPhaseStartEvent,
    StandbyPhase -> StandbyPhaseStartEvent,
    MainPhase -> MainPhaseStartEvent,
    BattlePhase -> BattlePhaseStartEvent,
    MainPhase2 -> MainPhase2StartEvent,
    EndPhase -> EndPhaseStartEvent
  )

  private val EndEvents: Map[Phase, PhaseEndEvent] = Map(
    DrawPhase -> DrawPhaseEndEvent,
    StandbyPhase -> StandbyPhaseEndEvent,
    MainPhase -> MainPhaseEndEvent,
    BattlePhase -> BattlePhaseEndEvent,
    MainPhase2 -> MainPhase2EndEvent,
    EndPhase -> EndPhaseEndEvent
  )

  def loop(implicit gameState: GameState) = {
    var phase: Phase = DrawPhase
    do {
      emit(StartEvents(phase))
      val nextPhase = phase.next(gameState.copy(phase = phase))
      emit(EndEvents(phase))
      phase = nextPhase
    } while (phase != null)
  }
}
