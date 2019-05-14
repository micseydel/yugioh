package yugioh.card.state

import yugioh.card.monster.Position
import yugioh.events.{EventsModule, TurnEndEvent}

/*
 * TODO: perhaps ControlledState should be something more like IdentifiableState?
 * For example, it usually applies to monsters not in the deck or hand. However, monsters which are in the grave
 * or banished are *not* considered "controlled" so this isn't quite right. Also, a set monster usually doesn't
 * have state, but if something like Geargiarmor flips then sets, it *does* retain some important state. Even
 * non-effect monsters matter however, as in a monster set by Book of Moon after attacking or manually changing position.
 */
trait ControlledState {
  def faceup: Boolean

  /**
    * Should be called when the class isn't use anymore. Ideally this would happen in a destructor, but they don't exist in Scala.
    */
  def close(): Unit // TODO: can probably implement a .finalize() method instead of this
}

case class SpellOrTrapControlledState(var faceup: Boolean) extends ControlledState {
  override def close(): Unit = ()
}

sealed trait AttackModifier

case class AttackDelta(value: Int) extends AttackModifier

case class MonsterControlledState(
  var position: Position,
  var attackedThisTurn: Boolean = false,
  var manuallyChangedPositionsThisTurn: Boolean = false, // also set to true if attacked
  var isPiercing: Boolean = false,
  var maybeAttackModifiers: List[AttackModifier] = Nil
)(implicit eventsModule: EventsModule) extends ControlledState {
  private[this] val subscription = eventsModule.observe {
    case TurnEndEvent =>
      attackedThisTurn = false
      manuallyChangedPositionsThisTurn = false
  }

  override def faceup: Boolean = Position.FaceUp.contains(position)

  override def close(): Unit = subscription.dispose()
}
