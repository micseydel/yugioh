package yugioh

import yugioh.action.DrawForTurnImpl
import yugioh.events.Observable.emit
import yugioh.events._

/**
  * State machine.
  */
sealed trait Phase {
  val abbreviation: String

  protected implicit val phase = this

  def next()(implicit gameState: GameState, turnPlayers: TurnPlayers): Phase
}


case object DrawPhase extends Phase {
  val abbreviation = "DP"

  protected implicit val fastEffectTiming = OpenGameState
  override def next()(implicit gameState: GameState, turnPlayers: TurnPlayers): Phase = {
    if (gameState.turnCount > 1) {
      (new DrawForTurnImpl).execute()
      FastEffectTiming.loop(start = CheckForTrigger)
    } else {
      FastEffectTiming.loop()
    }

    StandbyPhase
  }
}

case object StandbyPhase extends Phase {
  val abbreviation = "SP"

  override def next()(implicit gameState: GameState, turnPlayers: TurnPlayers): Phase = {
    FastEffectTiming.loop()
    MainPhase
  }
}

case object MainPhase extends Phase {
  val abbreviation = "MP"

  /**
    * If it's later than the first turn, ask the turn player if they want to go to BP. Otherwise go to EP.
    */
  override def next()(implicit gameState: GameState, turnPlayers: TurnPlayers): Phase = {
    FastEffectTiming.loop()
    if (gameState.turnCount > 1 && turnPlayers.turnPlayer.enterBattlePhase) {
      BattlePhase
    } else {
      EndPhase
    }
  }
}

case object BattlePhase extends Phase {
  val abbreviation = "BP"

  override def next()(implicit gameState: GameState, turnPlayers: TurnPlayers): Phase = {
    BattlePhaseStep.loop()
    MainPhase2
  }
}

case object MainPhase2 extends Phase {
  val abbreviation = "MP2"

  override def next()(implicit gameState: GameState, turnPlayers: TurnPlayers): Phase = {
    FastEffectTiming.loop()
    EndPhase
  }
}

case object EndPhase extends Phase {
  val abbreviation = "EP"

  override def next()(implicit gameState: GameState, turnPlayers: TurnPlayers): Phase = {
    FastEffectTiming.loop()
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

  def loop()(implicit gameState: GameState, turnPlayers: TurnPlayers) = {
    implicit var phase: Phase = DrawPhase
    do {
      emit(StartEvents(phase))
      val nextPhase = phase.next
      emit(EndEvents(phase))
      phase = nextPhase
    } while (phase != null)
  }
}


sealed trait Step

sealed trait BattlePhaseStep extends Step {
  protected implicit val phase: Phase = BattlePhase
  protected val nextStep: BattlePhaseStep
  protected implicit val implicitStep: Step = this

  def next(implicit gameState: GameState, turnPlayers: TurnPlayers): BattlePhaseStep = {
    FastEffectTiming.loop()
    nextStep
  }
}

object BattlePhaseStep {
  private val StartEvents: Map[BattlePhaseStep, BattlePhaseStepStartEvent] = Map(
    StartStep -> StartStepStepStartEvent,
    BattleStep -> BattleStepStepStartEvent,
    DamageStep -> DamageStepStepStartEvent,
    EndStep -> EndStepStepStartEvent
  )

  private val EndEvents: Map[BattlePhaseStep, BattlePhaseStepEndEvent] = Map(
    StartStep -> StartStepStepEndEvent,
    BattleStep -> BattleStepStepEndEvent,
    DamageStep -> DamageStepStepEndEvent,
    EndStep -> EndStepStepEndEvent
  )

  def loop()(implicit gameState: GameState, turnPlayers: TurnPlayers) = {
    var battlePhaseStep: BattlePhaseStep = StartStep
    do {
      emit(StartEvents(battlePhaseStep))
      val nextBattlePhaseStep = battlePhaseStep.next
      emit(EndEvents(battlePhaseStep))
      battlePhaseStep = nextBattlePhaseStep
    } while (battlePhaseStep != null)
  }
}

case object StartStep extends BattlePhaseStep {
  override protected val nextStep: BattlePhaseStep = BattleStep
}

case object BattleStep extends BattlePhaseStep {
  // TODO: DamageStep is also possible from this state, must override next method
  override protected val nextStep: BattlePhaseStep = EndStep
}

case object DamageStep extends BattlePhaseStep {
  override protected val nextStep: BattlePhaseStep = BattleStep
}

case object EndStep extends BattlePhaseStep {
  override protected val nextStep: BattlePhaseStep = null
}


sealed trait DamageStepSubStep extends Step

object DamageStepSubStep {
  private implicit val phase: Phase = BattlePhase

  val subStepsWithEvents: Seq[(DamageSubStepStartEvent, DamageStepSubStep, DamageSubStepEndEvent)] = Seq(
    (DamageSubStep1StartEvent, SubStep1, DamageSubStep1EndEvent),
    (DamageSubStep2StartEvent, SubStep2, DamageSubStep2EndEvent),
    (DamageSubStep3StartEvent, SubStep3, DamageSubStep3EndEvent),
    (DamageSubStep4StartEvent, SubStep4, DamageSubStep4EndEvent),
    (DamageSubStep5StartEvent, SubStep5, DamageSubStep5EndEvent),
    (DamageSubStep6StartEvent, SubStep6, DamageSubStep6EndEvent),
    (DamageSubStep7StartEvent, SubStep7, DamageSubStep7EndEvent)
  )

  def loop()(implicit gameState: GameState, turnPlayers: TurnPlayers): Unit = {
    for ((startEvent, subStep, endEvent) <- subStepsWithEvents) {
      implicit val implicitSubStep = subStep
      emit(startEvent)
      FastEffectTiming.loop()
      emit(endEvent)
    }
  }
}

case object SubStep1 extends DamageStepSubStep
case object SubStep2 extends DamageStepSubStep
case object SubStep3 extends DamageStepSubStep
case object SubStep4 extends DamageStepSubStep
case object SubStep5 extends DamageStepSubStep
case object SubStep6 extends DamageStepSubStep
case object SubStep7 extends DamageStepSubStep

