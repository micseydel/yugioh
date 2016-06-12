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


sealed trait BattlePhaseStepStartEvent extends StepStartEvent
sealed trait BattlePhaseStepEndEvent extends StepEndEvent

// I am so sorry for these names
case object StartStepStepStartEvent extends BattlePhaseStepStartEvent
case object StartStepStepEndEvent extends BattlePhaseStepEndEvent

case object BattleStepStepStartEvent extends BattlePhaseStepStartEvent
case object BattleStepStepEndEvent extends BattlePhaseStepEndEvent

case object DamageStepStepStartEvent extends BattlePhaseStepStartEvent
case object DamageStepStepEndEvent extends BattlePhaseStepEndEvent

case object EndStepStepStartEvent extends BattlePhaseStepStartEvent
case object EndStepStepEndEvent extends BattlePhaseStepEndEvent


case class DamageSubStepStartEvent(damageStepSubStep: DamageStepSubStep) extends BattlePhaseStepStartEvent
case class DamageSubStepEndEvent(damageStepStep: DamageStepSubStep) extends BattlePhaseStepEndEvent
