package yugioh.action

import yugioh.card.Card.AnyCard
import yugioh.card.SpellOrTrap
import yugioh.events.EventsModule
import yugioh.{GameState, Player}

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
  override def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = ()
}

sealed trait CardMoved

trait PlaceOnField extends InherentAction with CardMoved

trait SetCard extends PlaceOnField

// inherent action
trait SetAsSpellOrTrap extends SetCard {
  val spellOrTrap: SpellOrTrap
}

class SetAsSpellOrTrapImpl(override val spellOrTrap: SpellOrTrap) extends SetAsSpellOrTrap {
  override val player: Player = spellOrTrap.Owner

  override val toString = s"SetAsSpellOrTrap($spellOrTrap)"

  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    if (player.field.hasFreeSpellOrTrapZone) {
      player.field.placeAsSpellOrTrap(spellOrTrap, faceup = false)
      spellOrTrap.maybeTurnSet = Some(gameState.turnCount)
    } else {
      throw new IllegalStateException(s"Tried to set $spellOrTrap but there was no space.")
    }
  }
}

trait Discard extends InherentAction

class DiscardImpl(override val player: Player, cards: Seq[AnyCard]) extends Discard {
  def this(player: Player, card: AnyCard) = {
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
  gameState.turnPlayers.turnPlayer.cardsToDiscardForHandSizeLimit
) with DiscardForHandSizeLimit

trait Draw extends InherentAction

class DrawImpl(override val player: Player, howMany: Int = 1) extends Draw {

  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    player.draw(howMany)
  }
}

trait DrawForTurn extends Draw

class DrawForTurnImpl(implicit gameState: GameState) extends DrawForTurn {
  val player: Player = gameState.turnPlayers.turnPlayer

  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    gameState.turnPlayers.turnPlayer.draw()
  }
}

trait Destroy extends InherentAction {
  val cards: Seq[AnyCard]
}


case class DestroyImpl(override val player: Player, override val cards: Seq[AnyCard]) extends Destroy {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
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
