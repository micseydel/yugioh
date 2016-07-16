package yugioh.action

import yugioh._
import yugioh.card.{Card, Effect}
import yugioh.events.{Event, EventsModule}

sealed trait Action extends Event {
  val player: Player

  private var previouslyCalled = false

  def execute()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Action = {
    if (previouslyCalled) {
      throw new IllegalStateException(s"Action $this was already executed.")
    }

    doAction()
    eventsModule.emit(this)
    gameState.history.append(this)
    previouslyCalled = true

    this
  }

  def undo()(implicit gameState: GameState): Unit = throw new NotImplementedError(s"Undo has not been implemented for $this")

  def redo()(implicit gameState: GameState): Unit = throw new NotImplementedError(s"Redo has not been implemented for $this")

  protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit
}

trait InherentAction extends Action {
  /**
    * If the action is composed by something else.
    */
  val maybeParent: Option[Action]
}

/**
  * This can either be a card or an effect activation (the former may include the latter).
  */
sealed trait ExistsInAChainAction extends Action {
  val card: Card
  def resolve()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule)
}

/**
  * Effect does not have to be activated for some continuous spells and traps.
  */
case class CardActivation(card: Card, maybeEffect: Option[Effect]) extends ExistsInAChainAction {
  override val player = card.Owner

  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    maybeEffect.foreach(_.Activation.activate())
  }

  override def resolve()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    maybeEffect.foreach(effect => effect.Resolution.resolve(this))
  }
}

case class EffectActivation(card: Card, effect: Effect, player: Player) extends ExistsInAChainAction {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    effect.Activation.activate()
  }

  override def resolve()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    effect.Resolution.resolve(this)
  }
}

case class PassPriority(player: Player) extends InherentAction {
  override def toString = "PassPriority"
  override def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = ()
  override val maybeParent: Option[Action] = None
}

trait SetCard extends InherentAction

trait Discard extends InherentAction

class DiscardImpl(override val player: Player, cards: Seq[Card], parent: Action = null) extends Discard {
  val maybeParent = Option(parent)

  def this(player: Player, card: Card) = {
    this(player, Seq(card))
  }

  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    for (card <- cards) {
      card.discard()
    }
  }
}

trait DiscardForHandSizeLimit extends Discard

class DiscardForHandSizeLimitImpl(implicit gameState: GameState) extends DiscardForHandSizeLimit {
  val player = gameState.turnPlayers.turnPlayer
  val maybeParent: Option[Action] = None

  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    for (choice <- player.cardToDiscardForHandSizeLimit) {
      choice.sendToGrave()
    }
  }
}

trait Draw extends InherentAction

class DrawImpl(override val player: Player, howMany: Int = 1, parent: Action = null) extends Draw {
  val maybeParent: Option[Action] = Option(parent)

  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    player.draw(howMany)
  }
}

trait DrawForTurn extends Draw

class DrawForTurnImpl(implicit gameState: GameState) extends DrawForTurn {
  val player = gameState.turnPlayers.turnPlayer
  val maybeParent: Option[Action] = None

  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    gameState.turnPlayers.turnPlayer.draw()
  }
}

trait Destroy extends InherentAction {
  val cards: Seq[Card]
}


case class DestroyImpl(override val player: Player, override val cards: Seq[Card], parent: Action = null) extends Destroy {
  val maybeParent = Option(parent)

  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    for (card <- cards) {
      card.destroy()
    }
  }
}
