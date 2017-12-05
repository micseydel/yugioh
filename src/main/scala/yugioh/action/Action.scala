package yugioh.action

import com.typesafe.scalalogging.Logger
import yugioh._
import yugioh.card.{Effect, SpellOrTrap, Trap}
import yugioh.events.{ActionEvent, EventsModule, TimeSeparationEvent}



/**
  * TODO: Intended long-term to use the Action design pattern. This needs to be sussed out better before diving into that though.
  */
sealed trait Action {
  val player: Player

  protected[this] var previouslyCalled = false

  def execute()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): ActionEvent
}

trait InherentAction extends Action {
  /**
    * Call doAction() and then emit the event.
    */
  protected def doActionAndEmitEvent()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): ActionEvent = {
    doAction()
    eventsModule.emit(this)
  }

  protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit

  def execute()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): ActionEvent = {
    if (previouslyCalled) {
      throw new IllegalStateException(s"Action $this was already executed.")
    }

    val event = doActionAndEmitEvent()
    previouslyCalled = true

    event
  }

  /**
    * Do these actions simultaneously.
    */
  def also(action: InherentAction): InherentAction = {
    new InherentAction {
      override val player: Player = InherentAction.this.player

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
      override val player: Player = InherentAction.this.player

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
      override val player: Player = InherentAction.this.player

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

  override def execute()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): ActionEvent = {
    Effect.Activation.execute()
  }
}

/**
  * Effect does not have to be activated for some continuous spells and traps.
  *
  * TODO: make effect optional for card activation, e.g. Ultimate Offering or Skull Lair
  */
case class CardActivation(Card: SpellOrTrap, player: Player) extends Activation {
  private val logger = Logger[CardActivation]

  override def execute()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): ActionEvent = {
    Card.location match {
      case InHand =>
        if (Card.isInstanceOf[Trap]) {
          // this itself is unusual, as cards like Typhoon are very rare
          // that said, if we got here then other code allowed it
          logger.debug(s"Trap $Card was activated from hand")
        }

        Card.controller.field.placeAsSpellOrTrap(Card, faceup = true)
      case _: InSpellTrapZone =>
        Card.maybeControlledState.get.faceup = true
      case _ =>
        // this shouldn't happen, since even cards like Breakthrough Skills will merely activate their effect when elsewhere
        throw new AssertionError(s"Card activations cannot occur from anywhere other than hand or field; $Card tried to activate from ${Card.location}")
    }

    super.execute()
  }

  override val Effect: Effect = {
    assert(Card.Effects.size == 1, "Card activation only supports single-effect cards.")
    Card.Effects.head
  }
}

case class EffectActivation(Effect: Effect, player: Player) extends Activation
