package yugioh.action

import yugioh.Player
import yugioh.card.Effect

/**
  * A cause for something to happen in the game. Identifies who / what is responsible for an action.
  */
trait Cause

object GameMechanics extends Cause

case class PlayerCause(player: Player) extends Cause

case class EffectCause(effect: Effect) extends Cause

case class InherentCause(inherentAction: InherentAction) extends Cause
