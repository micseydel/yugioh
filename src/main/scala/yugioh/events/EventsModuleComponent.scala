package yugioh.events

import yugioh.action.Action
import yugioh.card.Effect

import scala.collection.mutable.ListBuffer

sealed trait Event

trait GamePlayEvent extends Event

case class ActionEvent(action: Action) extends Event

/**
  * Event to indicate a progression of time and separate events from being considered simultaneous.
  */
object TimeSeparationEvent extends Event

case class EffectActivationNegationEvent(negated: Effect, negater: Effect) extends Event

trait EventsModuleComponent {
  implicit def eventsModule: EventsModule
}

trait EventsModule {
  def observe(onEvent: PartialFunction[Event, Unit]): Subscription

  def emit[E <: Event](event: E): E

  def emit(action: Action): ActionEvent
}

trait Observer {
  def notify(event: Event): Unit
}

trait Subscription {
  def dispose(): Unit
}

object DefaultEventsModule extends EventsModule {
  val observers = new ListBuffer[Observer]

  private[this] def observe(observer: Observer): Subscription = {
    observers.append(observer)

    () => observers.remove(observers.indexOf(observer))
  }

  override def observe(onEvent: PartialFunction[Event, Unit]): Subscription = {
    observe((event: Event) => {
      if (onEvent.isDefinedAt(event)) {
        onEvent(event)
      }
    })
  }

  override def emit[E <: Event](event: E): E = {
    for (observer <- observers) {
      observer.notify(event)
    }

    event
  }

  override def emit(action: Action): ActionEvent = {
    emit(ActionEvent(action))
  }
}

trait DefaultEventsModuleComponent extends EventsModuleComponent {
  def eventsModule = DefaultEventsModule
}
