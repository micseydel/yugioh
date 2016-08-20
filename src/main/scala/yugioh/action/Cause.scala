package yugioh.action

/**
  * A cause for something to happen in the game. Is either a Player or GameMechanics.
  */
trait Cause

object GameMechanics extends Cause
