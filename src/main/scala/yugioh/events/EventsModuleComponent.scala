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
  def observe[E <: Event](observer: Observer[E]): Subscription

  def observe[E <: Event](onEvent: Event => Unit): Subscription

  def emit(event: Event): Event

  def emit(action: Action): ActionEvent
}

trait Observer[E <: Event] {
  def notify(event: Event): Unit
}

trait Subscription {
  def dispose(): Unit
}

object DefaultEventsModule extends EventsModule {
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

  def emit(event: Event) = {
    for (observer <- observers) {
      observer.notify(event)
    }

    event
  }

  def emit(action: Action) = {
    emit(ActionEvent(action)).asInstanceOf[ActionEvent]
  }
}

trait DefaultEventsModuleComponent extends EventsModuleComponent {
  def eventsModule = DefaultEventsModule
}
