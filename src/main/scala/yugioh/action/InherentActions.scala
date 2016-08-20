package yugioh.action

import yugioh.{GameState, Player}
import yugioh.card.Card
import yugioh.events.EventsModule

// purely for IDE navigation
private[this] object InherentActions

/**
  * Inert class to represent no action being taken.
  */
case class NoAction(player: Player) extends InherentAction {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = ()
}

case class PassPriority(player: Player) extends InherentAction {
  override def toString = "PassPriority"
  override def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = ()
}

trait PlaceOnField extends InherentAction // TODO

trait SetCard extends InherentAction

trait Discard extends InherentAction

class DiscardImpl(override val player: Player, cards: Seq[Card]) extends Discard {
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

class DiscardForHandSizeLimitImpl(implicit gameState: GameState) extends DiscardImpl(
  gameState.turnPlayers.turnPlayer,
  gameState.turnPlayers.turnPlayer.cardToDiscardForHandSizeLimit
) with DiscardForHandSizeLimit

trait Draw extends InherentAction

class DrawImpl(override val player: Player, howMany: Int = 1) extends Draw {

  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    player.draw(howMany)
  }
}

trait DrawForTurn extends Draw

class DrawForTurnImpl(implicit gameState: GameState) extends DrawForTurn {
  val player = gameState.turnPlayers.turnPlayer

  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    gameState.turnPlayers.turnPlayer.draw()
  }
}

trait Destroy extends InherentAction {
  val cards: Seq[Card]
}


case class DestroyImpl(override val player: Player, override val cards: Seq[Card]) extends Destroy {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    for (card <- cards) {
      card.destroy()
    }
  }
}

/**
  * Use negative lifepoints change to subtract lifepoints.
  */
case class ChangeLifePoints(lifePointsChange: Int, player: Player) extends InherentAction {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    player.lifePoints += lifePointsChange
  }
}
