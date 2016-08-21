package yugioh.action.monster

import yugioh._
import yugioh.action.{ActionModule, InherentAction}
import yugioh.card.monster.Monster
import yugioh.events.{EventsModule, GamePlayEvent}

sealed trait DeclareAttack extends InherentAction {
  val attacker: Monster
  val player = attacker.controller
}

case class DeclareAttackOnMonster(attacker: Monster, target: Monster) extends DeclareAttack {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = ()
}

case class DeclareDirectAttack(attacker: Monster) extends DeclareAttack {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = ()
}

case class Battle(attacker: Monster, maybeTarget: Option[Monster]) extends GamePlayEvent

case class Replay(attacker: Monster) extends GamePlayEvent
