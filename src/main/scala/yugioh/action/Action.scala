package yugioh.action

import yugioh._
import yugioh.card.{Effect, EffectCard}
import yugioh.events.{Event, EventsModule, TimeSeparationEvent}

/**
  * TODO: Intended long-term to use the Action design pattern. This needs to be sussed out better before diving into that though.
  */
// TODO: refactor Actions to not be Events, rather, for events to be created and emitted as a result of actions being executed.
sealed trait Action extends Event {
  val player: Player

  protected[this] var previouslyCalled = false

  def execute()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Action
}

trait InherentAction extends Action {
  /**
    * Call doAction() and then emit the event.
    */
  protected def doActionAndEmitEvent()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    doAction()
    eventsModule.emit(this)
  }

  protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit

  def execute()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Action = {
    if (previouslyCalled) {
      throw new IllegalStateException(s"Action $this was already executed.")
    }

    doActionAndEmitEvent()
    previouslyCalled = true

    this
  }

  /**
    * Do these actions simultaneously.
    */
  def also(action: InherentAction): InherentAction = {
    new InherentAction {
      override val player = InherentAction.this.player

      override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
        InherentAction.this.doAction()
        action.doAction()
      }
    }
  }

  /**
    * TODO - detect the "if you do" part
    * Perform `action` iff `this` executes successfully.
    */
  def andIfYouDo(action: InherentAction): InherentAction = {
    new InherentAction {
      override val player = InherentAction.this.player

      override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
        InherentAction.this.doActionAndEmitEvent() // TODO: detect success before executing the next action
        action.doActionAndEmitEvent()
      }
    }
  }

  /**
    * Perform `this` then in serial rather than simultaneously `action`.
    */
  def andThen(action: InherentAction): InherentAction = {
    new InherentAction {
      override val player = InherentAction.this.player

      override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
        InherentAction.this.doAction()
        eventsModule.emit(TimeSeparationEvent)
        action.doAction()
      }
    }
  }
}

/**
  * This can either be a card or an effect activation (the former may include the latter).
  */
sealed trait Activation extends Action {
  val Effect: Effect

  override def execute()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Action = {
    Effect.Activation.execute()
    this
  }
}

/**
  * Effect does not have to be activated for some continuous spells and traps.
  *
  * TODO: make effect optional for card activation
  */
case class CardActivation(Card: EffectCard, player: Player) extends Activation {
  override val Effect: Effect = {
    assert(Card.Effects.size == 1)
    Card.Effects.head
  }
}

case class EffectActivation(Effect: Effect, player: Player) extends Activation
