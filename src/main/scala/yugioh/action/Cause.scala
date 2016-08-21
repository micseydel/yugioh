package yugioh.action

/**
  * A cause for something to happen in the game.
  *
  * Is currently either a Player or GameMechanics, but may need to also include whether something is a cost or effect as well.
  */
trait Cause

object GameMechanics extends Cause
