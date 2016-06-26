package yugioh

trait GameLoss extends Exception {
  val loser: Player
}

case class OutOfLifepoints(loser: Player) extends GameLoss
case class EmptyDeck(loser: Player) extends GameLoss
