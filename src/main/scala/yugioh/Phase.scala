package yugioh

import yugioh.action.DrawForTurnImpl
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
      FastEffectTiming.loop(newGameState, start = CheckForTrigger(List(drawForTurn)))
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


object Phase extends DefaultEventsComponent {
  val phases = Seq(DrawPhase, StandbyPhase, MainPhase, BattlePhase, MainPhase2, EndPhase)
  val mainPhases = Set(MainPhase, MainPhase2)

  def loop(implicit gameState: GameState) = {
    var phase: Phase = DrawPhase
    do {
      events.emit(PhaseChangeEvent.StartEvents(phase))
      val nextPhase = phase.next(gameState.copy(phase = phase))
      events.emit(PhaseChangeEvent.EndEvents(phase))
      phase = nextPhase
    } while (phase != null)
  }
}
