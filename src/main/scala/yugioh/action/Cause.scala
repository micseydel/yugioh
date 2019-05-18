package yugioh.action

import yugioh.Player
import yugioh.card.Effect

/**
  * A cause for something to happen in the game. Identifies who / what is responsible for an action.
  */
trait Cause

case object GameMechanics extends Cause

case class PlayerCause(player: Player) extends Cause

// an effect is always caused by something else
case class EffectCause(effect: Effect, cause: Cause) extends Cause

object EffectCause {
  def apply(effect: Effect, player: Player): EffectCause = EffectCause(effect, player)
}

case class InherentCause(inherentAction: InherentAction) extends Cause

// TODO: there should probably be a BattleDamage cause, or something
