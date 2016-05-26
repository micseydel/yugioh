package yugioh

import yugioh.action.Action
import yugioh.card.{Card, SpellOrTrap}
import yugioh.card.monster.{Monster, PendulumMonster}
import yugioh.card.spell.FieldSpell

import scala.collection.mutable.ListBuffer

trait Field {
  // try to put things toward the center first
  protected val PositionPriorities = Seq(2, 1, 3, 0, 4)

  val MonsterZones: Array[Option[Monster]] = Array.fill(5)(None)
  val SpellTrapZones: Array[Option[SpellOrTrap]] = Array.fill(5)(None)

  val Graveyard = new ListBuffer[Card]
  val Banished = new ListBuffer[Card]

  var FieldSpellZone: Option[FieldSpell] = None

  var LeftPendulumZone: Option[PendulumMonster] = None
  var RightPendulumZone: Option[PendulumMonster] = None

  def hasFreeMonsterZone: Boolean = MonsterZones.exists(_.isEmpty)

  def hasFreeSpellOrTrapZone: Boolean = SpellTrapZones.exists(_.isEmpty)

  def placeAsMonster(monster: Monster, positionPreference: Option[Int] = None): InMonsterZone
  def placeAsSpellOrTrap(spellOrTrap: SpellOrTrap, positionPreference: Option[Int] = None): InSpellTrapZone

  /**
    * All the actions associated with a field, which includes grave and banished; may be empty.
    */
  def actions(implicit gameState: GameState, turnPlayer: Player, phase: Phase, step: Step = null) : Seq[Action]
}

class FieldImpl extends Field {
  override def placeAsMonster(monster: Monster, locationPreference: Option[Int]) = {
    placeAsHelper(MonsterZones, InMonsterZone.MonsterZones)(monster, locationPreference)
  }

  override def placeAsSpellOrTrap(spellOrTrap: SpellOrTrap, locationPreference: Option[Int]) = {
    placeAsHelper(SpellTrapZones, InSpellTrapZone.SpellTrapZones)(spellOrTrap, locationPreference)
  }

  private def placeAsHelper[C <: Card, Z <: InFieldZone](destinationArray: Array[Option[C]], destinationLocation: Seq[Z])
                                                        (card: C, locationPreference: Option[Int]): Z = {
    val position = locationPreference.getOrElse(PositionPriorities.filter(destinationArray(_).isEmpty).head)
    destinationArray.update(position, Some(card))
    destinationLocation(position)
  }

  override def actions(implicit gameState: GameState, turnPlayer: Player, phase: Phase, step: Step) = {
    ((MonsterZones ++ SpellTrapZones :+ FieldSpellZone :+ LeftPendulumZone :+ RightPendulumZone).flatten
      ++ Graveyard ++ Banished).flatMap(_.actions)
  }
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

object InMonsterZone {
  val MonsterZones: Seq[InMonsterZone] = Seq(InMonster0, InMonster1, InMonster2, InMonster3, InMonster4)
}

sealed trait InSpellTrapZone extends InFieldZone
case object InSpellTrap0 extends InSpellTrapZone
case object InSpellTrap1 extends InSpellTrapZone
case object InSpellTrap2 extends InSpellTrapZone
case object InSpellTrap3 extends InSpellTrapZone
case object InSpellTrap4 extends InSpellTrapZone

object InSpellTrapZone {
  val SpellTrapZones: Seq[InSpellTrapZone] = Seq(InSpellTrap0, InSpellTrap1, InSpellTrap2, InSpellTrap3, InSpellTrap4)
}

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
