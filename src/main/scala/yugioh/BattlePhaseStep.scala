package yugioh

import yugioh.action.monster.{DeclareAttack, TargetedForAttack}
import yugioh.card.monster.Monster
import yugioh.events.Observable._
import yugioh.events._

sealed trait Step

sealed trait BattlePhaseStep extends Step {
  protected val nextStep: BattlePhaseStep

  def next(gameState: GameState): BattlePhaseStep = {
    FastEffectTiming.loop(gameState.copy(step = this))
    nextStep
  }

  protected def emitStartEvent(): Unit
  protected def emitEndEvent(): Unit
}

object BattlePhaseStep {
  def loop(gameState: GameState) = {
    var battlePhaseStep: BattlePhaseStep = StartStep
    do {
      battlePhaseStep.emitStartEvent()
      val nextBattlePhaseStepAndMonster = battlePhaseStep.next(gameState)
      battlePhaseStep.emitEndEvent()
      battlePhaseStep = nextBattlePhaseStepAndMonster
    } while (battlePhaseStep != null)
  }
}

case object StartStep extends BattlePhaseStep {
  override protected val nextStep: BattlePhaseStep = BattleStep

  override def emitStartEvent(): Unit = emit(StartStepStepStartEvent)
  override def emitEndEvent(): Unit = emit(StartStepStepEndEvent)
}

case object BattleStep extends BattlePhaseStep {
  override protected val nextStep: BattlePhaseStep = null // not used here

  override def emitStartEvent(): Unit = emit(BattleStepStepStartEvent)
  override def emitEndEvent(): Unit = emit(BattleStepStepEndEvent)

  override def next(gameState: GameState): BattlePhaseStep = {
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
    FastEffectTiming.loop(gameState.copy(step = this))

    subscription.dispose()

    if (attacker != null) {
      DamageStep(attacker, target)
    } else {
      EndStep
    }
  }
}

case class DamageStep(attacker: Monster, target: Monster) extends BattlePhaseStep {
  override protected val nextStep: BattlePhaseStep = BattleStep

  override def emitStartEvent(): Unit = emit(DamageStepStepStartEvent)
  override def emitEndEvent(): Unit = emit(DamageStepStepEndEvent)

  override def next(gameState: GameState): BattlePhaseStep = {
    DamageStepSubStep.loop(attacker, target)(gameState.copy(step = this))
    nextStep
  }
}

case object EndStep extends BattlePhaseStep {
  override protected val nextStep: BattlePhaseStep = null

  override def emitStartEvent(): Unit = emit(EndStepStepStartEvent)
  override def emitEndEvent(): Unit = emit(EndStepStepEndEvent)
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
      FastEffectTiming.loop(gameState.copy(step = subStep))
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

