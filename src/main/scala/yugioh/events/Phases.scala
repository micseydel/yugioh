package yugioh.events

import yugioh._

case class TurnStartEvent(turnPlayers: TurnPlayers, mutableGameState: MutableGameState) extends GamePlayEvent

sealed case class PhaseStartEvent(phase: Phase) extends GamePlayEvent

sealed case class PhaseEndEvent(phase: Phase) extends GamePlayEvent

/**
  * Singleton objects to represent phase start/end.
  */
object PhaseChangeEvent {
  val StartEvents: Map[Phase, PhaseStartEvent] = Phase.Phases.map(phase => (phase, PhaseStartEvent(phase))).toMap
  val EndEvents: Map[Phase, PhaseEndEvent] = Phase.Phases.map(phase => (phase, PhaseEndEvent(phase))).toMap
}

case object TurnEndEvent extends GamePlayEvent


sealed trait StepStartEvent extends GamePlayEvent
sealed trait StepEndEvent extends GamePlayEvent


sealed case class BattlePhaseStepStartEvent(step: BattlePhaseStep) extends StepStartEvent
sealed case class BattlePhaseStepEndEvent(step: BattlePhaseStep) extends StepEndEvent


case class DamageSubStepStartEvent(damageStepSubStep: DamageStepSubStep) extends StepStartEvent
case class DamageSubStepEndEvent(damageStepStep: DamageStepSubStep) extends StepEndEvent
