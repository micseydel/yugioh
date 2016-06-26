package yugioh.events

import yugioh._

case class TurnStartEvent(turnPlayers: TurnPlayers, mutableGameState: MutableGameState) extends Event

sealed case class PhaseStartEvent(phase: Phase) extends Event

sealed case class PhaseEndEvent(phase: Phase) extends Event

/**
  * Singleton objects to represent phase start/end.
  */
object PhaseChangeEvent {
  val StartEvents: Map[Phase, PhaseStartEvent] = Phase.phases.map(phase => (phase, PhaseStartEvent(phase))).toMap
  val EndEvents: Map[Phase, PhaseEndEvent] = Phase.phases.map(phase => (phase, PhaseEndEvent(phase))).toMap
}

case object TurnEndEvent extends Event


sealed trait StepStartEvent extends Event
sealed trait StepEndEvent extends Event


sealed case class BattlePhaseStepStartEvent(step: BattlePhaseStep) extends StepStartEvent
sealed case class BattlePhaseStepEndEvent(step: BattlePhaseStep) extends StepEndEvent


case class DamageSubStepStartEvent(damageStepSubStep: DamageStepSubStep) extends StepStartEvent
case class DamageSubStepEndEvent(damageStepStep: DamageStepSubStep) extends StepEndEvent
