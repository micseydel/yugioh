package yugioh

import yugioh.action.{DiscardImpl, DrawForTurnImpl}

/**
  * State machine.
  */
sealed trait Phase {
  val abbreviation: String

  protected implicit val phase = this

  def next(implicit gameState: GameState, turnPlayer: Player): Phase
}

case object DrawPhase extends Phase {
  val abbreviation = "DP"
  override def next(implicit gameState: GameState, turnPlayer: Player): Phase = {
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
  override def next(implicit gameState: GameState, turnPlayer: Player): Phase = {
    FastEffectTiming.loop()
    MainPhase
  }
}

case object MainPhase extends Phase {
  val abbreviation = "MP"

  /**
    * If it's later than the first turn, ask the turn player if they want to go to BP. Otherwise go to EP.
    */
  override def next(implicit gameState: GameState, turnPlayer: Player): Phase = {
    FastEffectTiming.loop()
    if (gameState.turnCount > 1 && turnPlayer.enterBattlePhase) {
      BattlePhase
    } else {
      EndPhase
    }
  }
}

case object BattlePhase extends Phase {
  val abbreviation = "BP"
  override def next(implicit gameState: GameState, turnPlayer: Player): Phase = {
    BattlePhaseStep.loop()
    MainPhase2
  }
}

case object MainPhase2 extends Phase {
  val abbreviation = "MP2"
  override def next(implicit gameState: GameState, turnPlayer: Player): Phase = EndPhase
}

case object EndPhase extends Phase {
  val abbreviation = "EP"
  override def next(implicit gameState: GameState, turnPlayer: Player): Phase = {
    while (turnPlayer.hand.size > Constants.HandSizeLimit) {
      val action = turnPlayer.chooseAction(Seq(new DiscardImpl))
      action.execute()
    }

    EndTurn
  }
}

/**
  * State machine exit sentinel.
  */
case object EndTurn extends Phase {
  override val abbreviation: String = "Sentinel"

  override def next(implicit gameState: GameState, turnPlayer: Player): Phase = throw new UnsupportedOperationException
}

object Phase {
  val phases = Seq(DrawPhase, StandbyPhase, MainPhase, BattlePhase, MainPhase2, EndPhase)
  val mainPhases = Set(MainPhase, MainPhase2)
}


sealed trait Step

sealed trait BattlePhaseStep extends Step {
  protected implicit val phase: Phase = BattlePhase
  protected val nextStep: BattlePhaseStep
  protected implicit val implicitStep: Step = this

  def next(implicit gameState: GameState, turnPlayer: Player): BattlePhaseStep = {
    FastEffectTiming.loop()
    nextStep
  }
}

object BattlePhaseStep {
  def loop()(implicit gameState: GameState, turnPlayer: Player): Unit = {

    var battlePhaseStep: BattlePhaseStep = StartStep
    while (battlePhaseStep != EndStep) {
      battlePhaseStep = battlePhaseStep.next
    }
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
  override protected val nextStep: BattlePhaseStep = ExitBattlePhase
}

/**
  * State machine exit sentinel.
  */
case object ExitBattlePhase extends BattlePhaseStep {
  override protected val nextStep: BattlePhaseStep = null
  override def next(implicit gameState: GameState, turnPlayer: Player) = throw new UnsupportedOperationException
}


sealed trait DamageStepSubStep extends Step

object DamageStepSubStep {
  private implicit val phase: Phase = BattlePhase

  val subSteps = Seq(SubStep1, SubStep2, SubStep3, SubStep4, SubStep5, SubStep6, SubStep7)

  def loop()(implicit gameState: GameState, turnPlayer: Player): Unit = {
    subSteps foreach { subStep =>
      implicit val implicitSubStep = subStep
      FastEffectTiming.loop()
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

