package yugioh.action

import yugioh._
import yugioh.card.{Card, Effect}
import yugioh.events.{DefaultEventsComponent, Event, EventsComponent}

sealed trait Action extends Event {
  self: EventsComponent =>

  val player: Player

  private var previouslyCalled = false

  def execute()(implicit gameState: GameState): Action = {
    if (previouslyCalled) {
      throw new IllegalStateException(s"Action $this was already executed.")
    }

    doAction()
    events.emit(this)
    gameState.history.append(this)
    previouslyCalled = true

    this
  }

  def undo()(implicit gameState: GameState): Unit = throw new NotImplementedError(s"Undo has not been implemented for $this")

  def redo()(implicit gameState: GameState): Unit = throw new NotImplementedError(s"Redo has not been implemented for $this")

  protected def doAction()(implicit gameState: GameState): Unit
}

trait InherentAction extends Action with DefaultEventsComponent {
  /**
    * If the action is composed by something else.
    */
  val maybeParent: Option[Action]
}

/**
  * This can either be a card or an effect activation (the former may include the latter).
  */
sealed trait ExistsInAChainAction extends Action with DefaultEventsComponent {
  val card: Card
  def resolve()(implicit gameState: GameState)
}

/**
  * Effect does not have to be activated for some continuous spells and traps.
  */
case class CardActivation(card: Card, maybeEffect: Option[Effect]) extends ExistsInAChainAction {
  override protected def doAction()(implicit gameState: GameState): Unit = maybeEffect.foreach(_.Activation.activate())
  override val player = card.Owner

  override def resolve()(implicit gameState: GameState): Unit = maybeEffect.foreach(effect => effect.Resolution.resolve(this))
}

case class EffectActivation(card: Card, effect: Effect, player: Player) extends ExistsInAChainAction {
  override protected def doAction()(implicit gameState: GameState): Unit = effect.Activation.activate()

  override def resolve()(implicit gameState: GameState): Unit = effect.Resolution.resolve(this)
}

case class PassPriority(player: Player) extends InherentAction {
  override def toString = "PassPriority"
  override def doAction()(implicit gameState: GameState) = ()
  override val maybeParent: Option[Action] = None
}

trait SetCard extends InherentAction

trait Discard extends InherentAction

class DiscardImpl(override val player: Player, cards: Seq[Card], parent: Action = null) extends Discard {
  val maybeParent = Option(parent)

  def this(player: Player, card: Card) = {
    this(player, Seq(card))
  }

  override protected def doAction()(implicit gameState: GameState): Unit = {
    for (card <- cards) {
      card.discard()
    }
  }
}

trait DiscardForHandSizeLimit extends Discard

class DiscardForHandSizeLimitImpl(implicit gameState: GameState) extends DiscardForHandSizeLimit {
  val player = gameState.turnPlayers.turnPlayer
  val maybeParent: Option[Action] = None

  override protected def doAction()(implicit gameState: GameState) = {
    for (choice <- player.cardToDiscardForHandSizeLimit) {
      choice.sendToGrave()
    }
  }
}

trait Draw extends InherentAction

class DrawImpl(override val player: Player, howMany: Int = 1, parent: Action = null) extends Draw {
  val maybeParent: Option[Action] = Option(parent)

  override protected def doAction()(implicit gameState: GameState): Unit = player.draw(howMany)
}

trait DrawForTurn extends Draw

class DrawForTurnImpl(implicit gameState: GameState) extends DrawForTurn {
  val player = gameState.turnPlayers.turnPlayer
  val maybeParent: Option[Action] = None

  override protected def doAction()(implicit gameState: GameState): Unit = {
    gameState.turnPlayers.turnPlayer.draw()
  }
}

trait Destroy extends InherentAction {
  val cards: Seq[Card]
}


case class DestroyImpl(override val player: Player, override val cards: Seq[Card], parent: Action = null) extends Destroy {
  val maybeParent = Option(parent)

  /**
    * If an action must be specified, use the constructor which allows multiple cards.
    *
    * This is, unfortunately, a limitation of Scala:
    * http://stackoverflow.com/questions/4652095/why-does-the-scala-compiler-disallow-overloaded-methods-with-default-arguments
    */
  def this(player: Player, card: Card) = {
    this(player, Seq(card), null)
  }

  override protected def doAction()(implicit gameState: GameState): Unit = {
    for (card <- cards) {
      card.destroy()
    }
  }
}
