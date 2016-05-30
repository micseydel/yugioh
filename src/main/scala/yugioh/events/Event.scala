package yugioh.events

import yugioh._

import scala.collection.mutable.ListBuffer

trait Event

object Observable {
  val observers = new ListBuffer[Observer[_ <: Event]]

  def observe[E <: Event](observer: Observer[E]): Subscription = {
    observers.append(observer)

    new Subscription {
      override def dispose(): Unit = observers.remove(observers.indexOf(observer))
    }
  }

  def observe[E <: Event](onEvent: Event => Unit): Subscription = {
    observe(new Observer[E] {
      override def notify(event: Event): Unit = onEvent(event)
    })
  }

  def emit(event: Event): Unit = {
    for (observer <- observers) {
      observer.notify(event)
    }
  }
}

trait Observer[E <: Event] {
  def notify(event: Event): Unit
}

trait Subscription {
  def dispose(): Unit
}


case class TurnStartEvent(turnPlayers: TurnPlayers, gameState: GameState) extends Event

sealed trait PhaseStartEvent extends Event {
  val phase: Phase
}

sealed trait PhaseEndEvent extends Event {
  val phase: Phase
}

case object DrawPhaseStartEvent extends PhaseStartEvent { val phase = DrawPhase }
case object DrawPhaseEndEvent extends PhaseEndEvent { val phase = DrawPhase }

case object StandbyPhaseStartEvent extends PhaseStartEvent { val phase = StandbyPhase }
case object StandbyPhaseEndEvent extends PhaseEndEvent { val phase = StandbyPhase }

case object MainPhaseStartEvent extends PhaseStartEvent { val phase = MainPhase }
case object MainPhaseEndEvent extends PhaseEndEvent { val phase = MainPhase }

case object BattlePhaseStartEvent extends PhaseStartEvent { val phase = BattlePhase }
case object BattlePhaseEndEvent extends PhaseEndEvent { val phase = BattlePhase }

case object MainPhase2StartEvent extends PhaseStartEvent { val phase = MainPhase2 }
case object MainPhase2EndEvent extends PhaseEndEvent { val phase = MainPhase2 }

case object EndPhaseStartEvent extends PhaseStartEvent { val phase = EndPhase }
case object EndPhaseEndEvent extends PhaseEndEvent { val phase = EndPhase }

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


sealed trait DamageSubStepStartEvent extends BattlePhaseStepStartEvent
sealed trait DamageSubStepEndEvent extends BattlePhaseStepEndEvent

case object DamageSubStep1StartEvent extends DamageSubStepStartEvent
case object DamageSubStep1EndEvent extends DamageSubStepEndEvent

case object DamageSubStep2StartEvent extends DamageSubStepStartEvent
case object DamageSubStep2EndEvent extends DamageSubStepEndEvent

case object DamageSubStep3StartEvent extends DamageSubStepStartEvent
case object DamageSubStep3EndEvent extends DamageSubStepEndEvent

case object DamageSubStep4StartEvent extends DamageSubStepStartEvent
case object DamageSubStep4EndEvent extends DamageSubStepEndEvent

case object DamageSubStep5StartEvent extends DamageSubStepStartEvent
case object DamageSubStep5EndEvent extends DamageSubStepEndEvent

case object DamageSubStep6StartEvent extends DamageSubStepStartEvent
case object DamageSubStep6EndEvent extends DamageSubStepEndEvent

case object DamageSubStep7StartEvent extends DamageSubStepStartEvent
case object DamageSubStep7EndEvent extends DamageSubStepEndEvent
