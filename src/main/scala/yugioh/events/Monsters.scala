package yugioh.events

import yugioh._
import yugioh.action.{ActionModule, ChangeLifePoints, InherentAction}
import yugioh.card.monster.Monster


case class CauseBattleDamage(attackingPlayer: Player, recipient: Player, lifePoints: Int) extends InherentAction {
  assert(lifePoints >= 0) // may want to be even more strict

  override val player: Player = attackingPlayer

  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    ChangeLifePoints(-lifePoints, recipient).execute()
  }
}

trait Flipped extends GamePlayEvent { // TODO: refactor to not extend this
  val monster: Monster
  val cause: Event
}

case class FlippedRegular(monster: Monster, cause: Event) extends Flipped
case class FlipWithoutFlipEffects(monster: Monster, cause: Event) extends Flipped

case class DestroyByBattle(monster: Monster, by: Monster) extends InherentAction {
  override val player: Player = by.controller

  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    // do nothing - the monster is considered destroyed by battle relatively early in the damage step, and sent to the grave later
  }
}
