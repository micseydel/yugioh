package yugioh

import yugioh.action.DrawForTurnImpl
import yugioh.action.monster.{DeclareAttack, TargetedForAttack}
import yugioh.card.monster.Monster
import yugioh.events.Observable.{emit, observe}
import yugioh.events._

/**
  * State machine.
  */
sealed trait Phase {
  val abbreviation: String

  protected implicit val phase = this

  def next(gameState: GameState): Phase
}


case object DrawPhase extends Phase {
  val abbreviation = "DP"

  protected implicit val fastEffectTiming = OpenGameState
  override def next(gameState: GameState): Phase = {
    val newGameState = gameState.copy(phase = this)

    if (gameState.mutableGameState.turnCount > 1) {
      (new DrawForTurnImpl).execute()(newGameState)
      FastEffectTiming.loop(start = CheckForTrigger)(newGameState)
    } else {
      FastEffectTiming.loop()(newGameState)
    }

    StandbyPhase
  }
}

case object StandbyPhase extends Phase {
  val abbreviation = "SP"

  override def next(gameState: GameState): Phase = {
    FastEffectTiming.loop()(gameState.copy(phase = this))
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

    FastEffectTiming.loop()(newGameState)
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
    BattlePhaseStep.loop()(gameState.copy(phase = this))
    MainPhase2
  }
}

case object MainPhase2 extends Phase {
  val abbreviation = "MP2"

  override def next(gameState: GameState): Phase = {
    FastEffectTiming.loop()(gameState.copy(phase = this))
    EndPhase
  }
}

case object EndPhase extends Phase {
  val abbreviation = "EP"

  override def next(gameState: GameState): Phase = {
    FastEffectTiming.loop()(gameState.copy(phase = this))
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


sealed trait Step

case class BattlePhaseStepAndMonsters(step: BattlePhaseStep, attacker: Monster, target: Monster)

sealed trait BattlePhaseStep extends Step {
  protected val nextStep: BattlePhaseStep

  def next(attacker: Monster, target: Monster)(gameState: GameState): BattlePhaseStepAndMonsters = {
    FastEffectTiming.loop()(gameState.copy(step = this))
    BattlePhaseStepAndMonsters(nextStep, null, null)
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

  def loop()(gameState: GameState) = {
    var battlePhaseStepAndMonster = BattlePhaseStepAndMonsters(StartStep, null, null)
    do {
      battlePhaseStepAndMonster match {
        case BattlePhaseStepAndMonsters(step, attacker, target) =>
          emit(StartEvents(step))
          val nextBattlePhaseStepAndMonster = step.next(attacker, target)(gameState)
          emit(EndEvents(step))
          battlePhaseStepAndMonster = nextBattlePhaseStepAndMonster
      }
    } while (battlePhaseStepAndMonster.step != null)
  }
}

case object StartStep extends BattlePhaseStep {
  override protected val nextStep: BattlePhaseStep = BattleStep
}

case object BattleStep extends BattlePhaseStep {
  override protected val nextStep: BattlePhaseStep = null // not used here

  override def next(attacker: Monster, target: Monster)
                   (gameState: GameState): BattlePhaseStepAndMonsters = {
    var attacker: Monster = null
    var target: Monster = null
    val subscription = observe { event =>
      event match {
        case targeted: TargetedForAttack =>
          target = targeted.target
        case declareAttack: DeclareAttack =>
          attacker = declareAttack.attacker
        case ignore =>
      }
    }

    // listen for an attack declaration here
    FastEffectTiming.loop()(gameState.copy(step = this))

    subscription.dispose()

    if (attacker != null) {
      BattlePhaseStepAndMonsters(DamageStep, attacker, target)
    } else {
      BattlePhaseStepAndMonsters(EndStep, null, null)
    }
  }
}

case object DamageStep extends BattlePhaseStep {
  override protected val nextStep: BattlePhaseStep = BattleStep

  override def next(attacker: Monster, target: Monster)
                   (gameState: GameState): BattlePhaseStepAndMonsters = {
    DamageStepSubStep.loop(attacker, target)(gameState.copy(step = this))
    BattlePhaseStepAndMonsters(nextStep, null, null)
  }
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

  def loop(attacker: Monster, target: Monster)(implicit gameState: GameState): Unit = {
    //TODO: damage step sub step loop
    for ((startEvent, subStep, endEvent) <- subStepsWithEvents) {
      implicit val implicitSubStep = subStep
      emit(startEvent)
      FastEffectTiming.loop()(gameState.copy(step = subStep))
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

