package yugioh

import yugioh.action.{DiscardImpl, DrawForTurnImpl, PassPriority}

/**
  * State machine.
  */
sealed trait Phase {
  val abbreviation: String

  protected implicit val phase = this
  protected implicit val step = None

  def next(implicit turnPlayer: Player, gameState: GameState): Phase
}

case object DrawPhase extends Phase {
  val abbreviation = "DP"
  override def next(implicit turnPlayer: Player, gameState: GameState): Phase = {
    (new DrawForTurnImpl).execute()

    var state: FastEffectTiming = CheckForTrigger
    while (state != EndPhaseOrStep) {
      state = state.next
    }

    turnPlayer.chooseAction(Seq(new DrawForTurnImpl)) match {
      case pass: PassPriority =>
        // TODO: need to check with the opp
        StandbyPhase
      case _ => DrawPhase
    }
  }
}

case object StandbyPhase extends Phase {
  val abbreviation = "SP"
  override def next(implicit turnPlayer: Player, gameState: GameState): Phase = MainPhase
}

case object MainPhase extends Phase {
  val abbreviation = "MP"

  /**
    * If it's later than the first turn, ask the turn player if they want to go to BP. Otherwise go to EP.
    */
  override def next(implicit turnPlayer: Player, gameState: GameState): Phase = {
    if (gameState.turnCount > 1 && turnPlayer.enterBattlePhase) {
      BattlePhase
    } else {
      EndPhase
    }
  }
}

case object BattlePhase extends Phase {
  val abbreviation = "BP"
  override def next(implicit turnPlayer: Player, gameState: GameState): Phase = {
    implicit var battlePhaseStep: BattlePhaseStep = StartStep
    while (battlePhaseStep != EndStep) {
      battlePhaseStep = battlePhaseStep.next
    }

    MainPhase2
  }
}

case object MainPhase2 extends Phase {
  val abbreviation = "MP2"
  override def next(implicit turnPlayer: Player, gameState: GameState): Phase = EndPhase
}

case object EndPhase extends Phase {
  val abbreviation = "EP"
  override def next(implicit turnPlayer: Player, gameState: GameState): Phase = {
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

  override def next(implicit turnPlayer: Player, gameState: GameState): Phase = throw new UnsupportedOperationException
}

object Phase {
  val phases = Seq(DrawPhase, StandbyPhase, MainPhase, BattlePhase, MainPhase2, EndPhase)
  val mainPhases = Set(MainPhase, MainPhase2)
}


sealed trait Step

sealed trait BattlePhaseStep extends Step {
  def next(implicit turnPlayer: Player): BattlePhaseStep
}

case object StartStep extends BattlePhaseStep {
  override def next(implicit turnPlayer: Player): BattlePhaseStep = BattleStep
}
case object BattleStep extends BattlePhaseStep {
  override def next(implicit turnPlayer: Player): BattlePhaseStep = {
    // TODO: DamageStep is also possible from this state
    EndStep
  }
}

case object DamageStep extends BattlePhaseStep {
  override def next(implicit turnPlayer: Player): BattlePhaseStep = {
    DamageStepSubStep.subSteps foreach { subStep =>
      // TODO
    }

    BattleStep
  }
}

case object EndStep extends BattlePhaseStep {
  override def next(implicit turnPlayer: Player): BattlePhaseStep = {
    // TODO
    EndBattlePhase
  }
}

/**
  * State machine exit sentinel.
  */
case object EndBattlePhase extends BattlePhaseStep {
  override def next(implicit turnPlayer: Player): BattlePhaseStep = throw new UnsupportedOperationException
}


sealed trait DamageStepSubStep extends Step

case object SubStep1 extends DamageStepSubStep
case object SubStep2 extends DamageStepSubStep
case object SubStep3 extends DamageStepSubStep
case object SubStep4 extends DamageStepSubStep
case object SubStep5 extends DamageStepSubStep
case object SubStep6 extends DamageStepSubStep
case object SubStep7 extends DamageStepSubStep

object DamageStepSubStep {
  val subSteps = Seq(SubStep1, SubStep2, SubStep3, SubStep4, SubStep5, SubStep6, SubStep7)
}
