package yugioh.events

import scala.collection.mutable.ListBuffer

trait Event

trait EventsComponent {
  def events: Events
}

trait Events {
  def observe[E <: Event](observer: Observer[E]): Subscription

  def observe[E <: Event](onEvent: Event => Unit): Subscription

  def emit(event: Event): Unit
}

trait Observer[E <: Event] {
  def notify(event: Event): Unit
}

trait Subscription {
  def dispose(): Unit
}

object DefaultEvents extends Events {
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

trait DefaultEventsComponent extends EventsComponent {
  def events = DefaultEvents
}
