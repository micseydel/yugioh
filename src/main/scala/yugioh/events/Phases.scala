package yugioh.events

import yugioh._

case class TurnStartEvent(turnPlayers: TurnPlayers, mutableGameState: MutableGameState) extends GamePlayEvent

sealed trait PhaseTransitionEvent extends GamePlayEvent

sealed case class PhaseStartEvent(phase: Phase) extends PhaseTransitionEvent

sealed case class PhaseEndEvent(phase: Phase) extends PhaseTransitionEvent

/**
  * Singleton objects to represent phase start/end.
  */
object PhaseChangeEvent {
  private def helper[T <: PhaseTransitionEvent](phaseTransitionEvent: Phase => T): Map[Phase, T] = {
    Phase.Phases.map(phase => (phase, phaseTransitionEvent(phase))).toMap
  }

  val StartEvents: Map[Phase, PhaseStartEvent] = helper(phase => PhaseStartEvent(phase))
  val EndEvents: Map[Phase, PhaseEndEvent] = helper(phase => PhaseEndEvent(phase))
}

case object TurnEndEvent extends GamePlayEvent


sealed trait StepStartEvent extends GamePlayEvent
sealed trait StepEndEvent extends GamePlayEvent


sealed case class BattlePhaseStepStartEvent(step: BattlePhaseStep) extends StepStartEvent
sealed case class BattlePhaseStepEndEvent(step: BattlePhaseStep) extends StepEndEvent


case class DamageSubStepStartEvent(damageStepSubStep: DamageStepSubStep) extends StepStartEvent
case class DamageSubStepEndEvent(damageStepStep: DamageStepSubStep) extends StepEndEvent
