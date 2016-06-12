package yugioh.action.monster

import yugioh._
import yugioh.action.InherentAction
import yugioh.card.monster.Monster
import yugioh.events.{DefaultEventsComponent, Event}

sealed trait DeclareAttack extends InherentAction {
  val attacker: Monster
}

case class DeclareAttackOnMonster(attacker: Monster) extends DeclareAttack with DefaultEventsComponent {
  override protected def doAction()(implicit gameState: GameState): Unit = {
    val turnPlayers = gameState.turnPlayers
    val attackTarget = turnPlayers.turnPlayer.selectAttackTarget(attacker, turnPlayers.opponent.field.monsterZones.toSeq.flatten)
    events.emit(TargetedForAttack(attacker, attackTarget))
  }
}

case class DeclareDirectAttack(attacker: Monster) extends DeclareAttack {
  override protected def doAction()(implicit gameState: GameState) = ()
}

case class TargetedForAttack(attacker: Monster, target: Monster) extends Event

/**
  * null target indicates a direct attack
  */
case class Battle(attacker: Monster, target: Monster) extends Event

case class Replay(attacker: Monster) extends Event
