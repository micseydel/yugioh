package yugioh

trait GameLoss extends Exception

trait OutOfLifepoints extends GameLoss
trait EmtpyDeck extends GameLoss

object GameLossImpl extends GameLoss

object OutOfLifepointsImpl extends OutOfLifepoints
object EmtpyDeckImpl extends EmtpyDeck
