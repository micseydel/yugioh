package yugioh.action

import yugioh._
import yugioh.card.{Card, Effect}
import yugioh.events.{DefaultEventsComponent, Event, EventsComponent}

trait Action extends Event {
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

trait InherentAction extends Action with DefaultEventsComponent

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

  override def resolve()(implicit gameState: GameState): Unit = maybeEffect.foreach(effect => effect.Resolution.resolve())
}

case class EffectActivation(card: Card, effect: Effect, player: Player) extends ExistsInAChainAction {
  override protected def doAction()(implicit gameState: GameState): Unit = effect.Activation.activate()

  override def resolve()(implicit gameState: GameState): Unit = effect.Resolution.resolve()
}

case class PassPriority(player: Player) extends InherentAction {
  override def toString = "PassPriority"
  override def doAction()(implicit gameState: GameState) = ()
}

trait SetCard extends InherentAction

trait Discard extends InherentAction

class DiscardImpl(card: Card) extends Discard {
  override val player: Player = card.controller

  override protected def doAction()(implicit gameState: GameState): Unit = card.discard()
}

trait DiscardForHandSizeLimit extends Discard

class DiscardForHandSizeLimitImpl(implicit gameState: GameState) extends DiscardForHandSizeLimit {
  val player = gameState.turnPlayers.turnPlayer

  override protected def doAction()(implicit gameState: GameState) = {
    for (choice <- player.cardToDiscardForHandSizeLimit) {
      choice.sendToGrave()
    }
  }
}

trait Draw extends InherentAction

class DrawImpl(override val player: Player, howMany: Int = 1) extends Draw {
  override protected def doAction()(implicit gameState: GameState): Unit = player.draw(howMany)
}

trait DrawForTurn extends Draw

class DrawForTurnImpl(implicit gameState: GameState) extends DrawForTurn {
  val player = gameState.turnPlayers.turnPlayer

  override protected def doAction()(implicit gameState: GameState): Unit = {
    gameState.turnPlayers.turnPlayer.draw()
  }
}
