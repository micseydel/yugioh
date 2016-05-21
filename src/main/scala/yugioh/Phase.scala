package yugioh

sealed trait Phase {
  val abbreviation: String
}

case object DrawPhase extends Phase { val abbreviation = "DP" }
case object StandbyPhase extends Phase { val abbreviation = "SP" }
case object MainPhase extends Phase { val abbreviation = "MP" }
case object BattlePhase extends Phase { val abbreviation = "BP" }
case object MainPhase2 extends Phase { val abbreviation = "MP2" }
case object EndPhase extends Phase { val abbreviation = "EP" }

object Phase {
  val phases = Seq(DrawPhase, StandbyPhase, MainPhase, BattlePhase, MainPhase2, EndPhase)
  val mainPhases = Set(MainPhase, MainPhase2)
}


sealed trait Step

sealed trait BattlePhaseStep extends Step

case object StartStep extends BattlePhaseStep
case object BattleStep extends BattlePhaseStep
case object DamageStep extends BattlePhaseStep
case object EndStep extends BattlePhaseStep

object BattlePhaseStep {
  val steps = Seq(StartStep, BattleStep, DamageStep, EndStep)
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
