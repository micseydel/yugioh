package yugioh.action.monster

import yugioh._
import yugioh.action.{ActionModule, InherentAction}
import yugioh.card.monster.Monster
import yugioh.events.{Event, EventsModule}

sealed trait DeclareAttack extends InherentAction {
  val attacker: Monster
  val player = attacker.controller
}

/**
  * Target is determined by asking the player when the action is performed.
  */
trait DeclareAttackOnMonster extends DeclareAttack

case class DeclareAttackOnMonsterImpl(attacker: Monster) extends DeclareAttackOnMonster {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    val turnPlayers = gameState.turnPlayers
    val attackTarget = turnPlayers.turnPlayer.selectAttackTarget(attacker, turnPlayers.opponent.field.monsterZones.toSeq.flatten)
    eventsModule.emit(TargetedForAttack(attacker, attackTarget))
  }
}

trait DeclareDirectAttack extends DeclareAttack

case class DeclareDirectAttackImpl(attacker: Monster) extends DeclareDirectAttack {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = ()
}

case class TargetedForAttack(attacker: Monster, target: Monster) extends Event

/**
  * null target indicates a direct attack
  */
case class Battle(attacker: Monster, target: Monster) extends Event

case class Replay(attacker: Monster) extends Event
