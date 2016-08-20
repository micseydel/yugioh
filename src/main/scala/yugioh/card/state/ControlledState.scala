package yugioh.card.state

import yugioh.card.monster.Position
import yugioh.events.{EventsModule, TurnEndEvent}

trait ControlledState {
  def faceup: Boolean

  /**
    * Should be called when the class isn't use anymore. Ideally this would happen in a destructor, but they don't exist in Scala.
    */
  def close(): Unit
}

case class SpellTrapControlledState(var faceup: Boolean) extends ControlledState {
  override def close() = ()
}

case class MonsterControlledState(
  var position: Position,
  var attackedThisTurn: Boolean = false,
  var manuallyChangedPositionsThisTurn: Boolean = false, // also set to true if attacked
  var isPiercing: Boolean = false
)(implicit eventsModule: EventsModule) extends ControlledState {
  private[this] val Subscription = eventsModule.observe { event =>
    event match {
      case TurnEndEvent =>
        attackedThisTurn = false
        manuallyChangedPositionsThisTurn = false
      case ignore =>
    }
  }

  override def faceup: Boolean = Position.FaceUp.contains(position)

  override def close() = Subscription.dispose()
}
