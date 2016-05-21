package yugioh

import yugioh.card.{Card, SpellOrTrap}
import yugioh.card.monster.Monster

import scala.collection.mutable.ListBuffer

trait Field {
  // try to put things toward the center first
  protected val PositionPriorities = Seq(2, 1, 3, 0, 4)

  val MonsterZones: Array[Option[Monster]] = new Array[Option[Monster]](5)
  val SpellTrapZones: Array[Option[SpellOrTrap]] = new Array[Option[SpellOrTrap]](5)
  val Graveyard = new ListBuffer[Card]
  val Banished = new ListBuffer[Card]

  def hasFreeMonsterZone: Boolean = MonsterZones.exists(_.isEmpty)

  def hasFreeSpellOrTrapZone: Boolean = SpellTrapZones.exists(_.isEmpty)

  // override methods
  def placeAsMonster(monster: Card, locationPreference: Option[Location])
  def placeAsSpellOrTrap(spellOrTrap: Card, locationPreference: Option[Location])

  def get(location: Location): Option[Card]
}

// TODO: fill in stump impl
class FieldImpl extends Field {
  // override methods
  override def placeAsMonster(monster: Card, locationPreference: Option[Location]): Unit = {}

  override def get(location: Location): Option[Card] = None

  override def placeAsSpellOrTrap(spellOrTrap: Card, locationPreference: Option[Location]): Unit = {}
}

sealed trait Location


case object InDeck extends Location
case object InHand extends Location
case object InGraveyard extends Location
case object InBanished extends Location

sealed trait InFieldZone extends Location

case object InFieldSpell extends InFieldZone

sealed trait InMonsterZone extends InFieldZone
case object InMonster0 extends InMonsterZone
case object InMonster1 extends InMonsterZone
case object InMonster2 extends InMonsterZone
case object InMonster3 extends InMonsterZone
case object InMonster4 extends InMonsterZone

sealed trait InSpellTrapZone extends InFieldZone
case object InSpellTrap0 extends InSpellTrapZone
case object InSpellTrap1 extends InSpellTrapZone
case object InSpellTrap2 extends InSpellTrapZone
case object InSpellTrap3 extends InSpellTrapZone
case object InSpellTrap4 extends InSpellTrapZone

sealed trait InPendulumZone extends InFieldZone
case object LeftInPendulumZone extends InPendulumZone
case object RightInPendulumZone extends InPendulumZone

object Location {
  val MonsterZones = Set(InMonster0, InMonster1, InMonster2, InMonster3, InMonster4)
  val SpellTrapZones = Set(InSpellTrap0, InSpellTrap1, InSpellTrap2, InSpellTrap3, InSpellTrap4)
  val PendulumZones = Set(LeftInPendulumZone, RightInPendulumZone)

  val FieldZones = Seq(MonsterZones, SpellTrapZones, Set(InFieldSpell), PendulumZones).map(_.asInstanceOf[Set[InFieldZone]]).reduce(_ & _)

  val Locations = FieldZones.asInstanceOf[Set[Location]] & Set(InDeck, InHand, InGraveyard, InBanished)
}
