package yugioh

import yugioh.action.monster.{DeclareAttack, TargetedForAttack}
import yugioh.card.monster.Monster
import yugioh.events.Observable._
import yugioh.events._

sealed trait Step

sealed trait BattlePhaseStep extends Step {
  protected def emitStartEvent(): Unit
  protected def emitEndEvent(): Unit

  def next(gameState: GameState): BattlePhaseStep
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
  override def emitStartEvent(): Unit = emit(StartStepStepStartEvent)
  override def emitEndEvent(): Unit = emit(StartStepStepEndEvent)

  override def next(gameState: GameState): BattlePhaseStep = {
    FastEffectTiming.loop(gameState.copy(step = this))
    BattleStep
  }
}

// TODO: replays
case object BattleStep extends BattlePhaseStep {
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
  override def emitStartEvent(): Unit = emit(DamageStepStepStartEvent)
  override def emitEndEvent(): Unit = emit(DamageStepStepEndEvent)

  override def next(gameState: GameState): BattlePhaseStep = {
    DamageStepSubStep.loop(attacker, target)(gameState.copy(step = this))
    BattleStep
  }
}

case object EndStep extends BattlePhaseStep {
  override def emitStartEvent(): Unit = emit(EndStepStepStartEvent)
  override def emitEndEvent(): Unit = emit(EndStepStepEndEvent)

  override def next(gameState: GameState): BattlePhaseStep = {
    FastEffectTiming.loop(gameState.copy(step = this))
    null
  }
}


sealed trait DamageStepSubStep extends Step

object DamageStepSubStep {
  val subSteps = Seq(
    StartOfTheDamageStep,
    BeforeDamageCalculation,
    PerformDamageCalculation,
    AfterDamageCalculation,
    EndOfThDamageStep
  )

  def loop(attacker: Monster, target: Monster)(implicit gameState: GameState): Unit = {
    for (subStep <- subSteps) {
      emit(DamageSubStepStartEvent(subStep))
      FastEffectTiming.loop(gameState.copy(step = subStep))
      emit(DamageSubStepEndEvent(subStep))
    }
  }
}

// http://www.yugioh-card.com/uk/gameplay/damage.html
case object StartOfTheDamageStep extends DamageStepSubStep
case object BeforeDamageCalculation extends DamageStepSubStep
case object PerformDamageCalculation extends DamageStepSubStep
case object AfterDamageCalculation extends DamageStepSubStep
case object EndOfThDamageStep extends DamageStepSubStep
