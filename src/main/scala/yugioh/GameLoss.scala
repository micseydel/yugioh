package yugioh

trait GameLoss extends Exception

trait OutOfLifepoints extends GameLoss
trait EmptyDeck extends GameLoss

object GameLossImpl extends GameLoss

object OutOfLifepointsImpl extends OutOfLifepoints
object EmptyDeckImpl extends EmptyDeck
