package yugioh.events

import yugioh.Player

/**
  * player is the player who is losing the life points.
  */
trait LifePointsLost extends Event {
  val player: Player
  val lifePoints: Int
}

trait LifePointsDamage extends LifePointsLost