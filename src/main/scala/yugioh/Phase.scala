package yugioh

import yugioh.action.DrawForTurnImpl
import yugioh.events._

/**
  * State machine.
  */
sealed trait Phase {
  val abbreviation: String

  def next(gameState: GameState)(implicit eventsModule: EventsModule, battlePhaseModule: BattlePhaseModule): Phase
}


case object DrawPhase extends Phase {
  val abbreviation = "DP"

  override def next(gameState: GameState)(implicit eventsModule: EventsModule, battlePhaseModule: BattlePhaseModule): Phase = {
    implicit val newGameState = gameState.copy(phase = this)

    if (gameState.mutableGameState.turnCount > 1) {
      val drawForTurn = new DrawForTurnImpl // TODO: decouple
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

  override def next(gameState: GameState)(implicit eventsModule: EventsModule, battlePhaseModule: BattlePhaseModule): Phase = {
    FastEffectTiming.loop(gameState.copy(phase = this))
    MainPhase
  }
}

case object MainPhase extends Phase {
  val abbreviation = "MP"

  /**
    * If it's later than the first turn, ask the turn player if they want to go to BP. Otherwise go to EP.
    */
  override def next(gameState: GameState)(implicit eventsModule: EventsModule, battlePhaseModule: BattlePhaseModule): Phase = {
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

  override def next(gameState: GameState)(implicit eventsModule: EventsModule, battlePhaseModule: BattlePhaseModule): Phase = {
    battlePhaseModule.loop(gameState.copy(phase = this))
    MainPhase2
  }
}

case object MainPhase2 extends Phase {
  val abbreviation = "MP2"

  override def next(gameState: GameState)(implicit eventsModule: EventsModule, battlePhaseModule: BattlePhaseModule): Phase = {
    FastEffectTiming.loop(gameState.copy(phase = this))
    EndPhase
  }
}

case object EndPhase extends Phase {
  val abbreviation = "EP"

  override def next(gameState: GameState)(implicit eventsModule: EventsModule, battlePhaseModule: BattlePhaseModule): Phase = {
    FastEffectTiming.loop(gameState.copy(phase = this))
    null
  }
}

object Phase {
  val Phases = Seq(DrawPhase, StandbyPhase, MainPhase, BattlePhase, MainPhase2, EndPhase)
  val MainPhases = Seq(MainPhase, MainPhase2)
}


trait PhaseModuleComponent {
  trait PhaseModule {
    def loop(implicit gameState: GameState)
  }

  def phaseModule: PhaseModule
}

trait DefaultPhaseModuleComponent extends PhaseModuleComponent {
  self: EventsModuleComponent
    with BattlePhaseModuleComponent =>

  override def phaseModule = new PhaseModule {
    def loop(implicit gameState: GameState) = {
      var phase: Phase = DrawPhase
      do {
        eventsModule.emit(PhaseChangeEvent.StartEvents(phase))
        val nextPhase = phase.next(gameState.copy(phase = phase))
        eventsModule.emit(PhaseChangeEvent.EndEvents(phase))
        phase = nextPhase
      } while (phase != null)
    }
  }
}
