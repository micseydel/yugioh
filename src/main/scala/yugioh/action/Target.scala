package yugioh.action

import yugioh.Location
import yugioh.action.card.CardMovedAction
import yugioh.card.Card.AnyCard
import yugioh.card.SpellOrTrap
import yugioh.card.monster.Monster
import yugioh.events.{ActionEvent, EventsModule, Subscription}

trait Target[Card <: AnyCard] {
  val card: Card
  val location: Location

  def validTarget: Boolean = _validTarget

  protected[this] val eventsModule: EventsModule

  protected[this] var _validTarget = true

  protected[this] val subscription: Subscription = eventsModule.observe {
    case ActionEvent(CardMovedAction(_, `card`, _, _)) =>
      _validTarget = false
      subscription.dispose()
  }
}

case class MonsterTarget(card: Monster, location: Location)
                        (implicit override val eventsModule: EventsModule) extends Target[Monster]

case class SpellOrTrapTarget(card: SpellOrTrap, location: Location)
                            (implicit override val eventsModule: EventsModule) extends Target[SpellOrTrap]
