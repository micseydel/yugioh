package yugioh.card.monster

sealed trait Position

// there is technically a way to be face down and in attack mode
case object Set extends Position // in defense mode
case object Attack extends Position
case object Defense extends Position

object Position {
  val FaceUp: Set[Position] = scala.collection.immutable.Set(Attack, Defense)
}
