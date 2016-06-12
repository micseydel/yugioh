package yugioh.events

import yugioh._
import yugioh.card.monster.Monster


case class BattleDamage(player: Player, lifePoints: Int) extends LifePointsDamage

trait Flipped extends Event {
  val monster: Monster
  val cause: Event
}

case class FlippedRegular(monster: Monster, cause: Event) extends Flipped
case class FlipWithoutFlipEffects(monster: Monster, cause: Event) extends Flipped

case class DestroyedByBattle(monster: Monster, by: Monster) extends Event
