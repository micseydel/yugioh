package yugioh

import yugioh.action.Action
import yugioh.card.monster.{Monster, PendulumMonster}
import yugioh.card.spell.FieldSpell
import yugioh.card.{Card, SpellOrTrap}

import scala.collection.mutable.ListBuffer

trait Field {
  // try to put things toward the center first
  protected val PositionPriorities = Seq(2, 1, 3, 0, 4)

  val monsterZones: Array[Option[Monster]] = Array.fill(5)(None)
  val spellTrapZones: Array[Option[SpellOrTrap]] = Array.fill(5)(None)

  val graveyard = new ListBuffer[Card]
  val banished = new ListBuffer[Card]

  var fieldSpellZone: Option[FieldSpell] = None

  var leftPendulumZone: Option[PendulumMonster] = None
  var rightPendulumZone: Option[PendulumMonster] = None

  def hasFreeMonsterZone: Boolean = monsterZones.exists(_.isEmpty)

  def hasFreeSpellOrTrapZone: Boolean = spellTrapZones.exists(_.isEmpty)

  def placeAsMonster(monster: Monster, positionPreference: Option[Int] = None): InMonsterZone
  def placeAsSpellOrTrap(spellOrTrap: SpellOrTrap, positionPreference: Option[Int] = None): InSpellTrapZone

  def sendToGrave(card: Card): Unit

  /**
    * All the actions associated with a field, which includes grave and banished; may be empty.
    */
  def actions(implicit gameState: GameState) : Seq[Action]
}

class FieldImpl extends Field {
  override def placeAsMonster(monster: Monster, locationPreference: Option[Int]) = {
    placeAsHelper(monsterZones, InMonsterZone.MonsterZones)(monster, locationPreference)
  }

  override def placeAsSpellOrTrap(spellOrTrap: SpellOrTrap, locationPreference: Option[Int]) = {
    placeAsHelper(spellTrapZones, InSpellTrapZone.SpellTrapZones)(spellOrTrap, locationPreference)
  }

  private def placeAsHelper[C <: Card, Z <: InFieldZone](destinationArray: Array[Option[C]], destinationLocation: Seq[Z])
                                                        (card: C, locationPreference: Option[Int]): Z = {
    val position = locationPreference.getOrElse(PositionPriorities.filter(destinationArray(_).isEmpty).head)
    destinationArray.update(position, Some(card))

    // TODO LOW: want to make this an implicit in the utils, clean up the two deck situations
    def remove[T](buffer: ListBuffer[T], element: T): Unit = buffer.remove(buffer.indexOf(element))

    // remove it from its previous location
    card.location match {
      case InHand => remove(card.owner.hand, card)
      case InGraveyard => remove(card.owner.field.graveyard, card)
      case InBanished => remove(card.owner.field.banished, card)
      case monsterZone: InMonsterZone => removeFromMonsterZone(monsterZone)
      case spellTrapZone: InSpellTrapZone => removeFromSpellTrapZone(spellTrapZone)
      case InFieldSpell => fieldSpellZone = None
      case InLeftPendulumZone => leftPendulumZone = None
      case InRightPendulumZone => rightPendulumZone = None
      case InDeck =>
        card.owner.deck.cards.remove(card.owner.deck.cards.indexOf(card))
        card.owner.deck.shuffle()
      case InExtraDeck =>
        card.owner.extraDeck.remove(card.owner.extraDeck.indexOf(card))
    }

    val toWhere = destinationLocation(position)
    card.location = toWhere
    toWhere
  }

  def removeFromMonsterZone[Z <: InMonsterZone](monsterZone: Z) = {
    removeFromHelper(monsterZones, monsterZone, InMonsterZone.MonsterZones)
  }

  def removeFromSpellTrapZone[Z <: InSpellTrapZone](spellTrapZone: Z) = {
    removeFromHelper(spellTrapZones, spellTrapZone, InSpellTrapZone.SpellTrapZones)
  }

  private def removeFromHelper[T](removeFrom: Array[Option[T]], zone: InFieldZone, zones: Seq[InFieldZone]) = {
    removeFrom.update(zones.indexOf(zone), None)
  }

  override def actions(implicit gameState: GameState) = {
    ((monsterZones ++ spellTrapZones :+ fieldSpellZone :+ leftPendulumZone :+ rightPendulumZone).flatten
      ++ graveyard ++ banished).flatMap(_.actions)
  }

  override def sendToGrave(card: Card): Unit = {
    // remove from current location
    card.location match {
      case InDeck =>
        card.owner.deck.cards.remove(card.owner.deck.cards.indexOf(card))
      case InHand =>
        card.owner.hand.remove(card.owner.hand.indexOf(card))
      case InBanished =>
        card.owner.field.banished.remove(card.owner.field.banished.indexOf(card))
      case InExtraDeck =>
        card.owner.extraDeck.remove(card.owner.extraDeck.indexOf(card))
      case InRightPendulumZone =>
        rightPendulumZone = None
      case InLeftPendulumZone =>
        leftPendulumZone = None
      case monsterZone: InMonsterZone =>
        monsterZones.update(InMonsterZone.MonsterZones.indexOf(monsterZone), None)
      case spellTrapZone: InSpellTrapZone =>
        spellTrapZones.update(InSpellTrapZone.SpellTrapZones.indexOf(spellTrapZone), None)
      case InFieldSpell =>
        fieldSpellZone = None
      case InGraveyard =>
        throw new IllegalArgumentException(s"Can't send to grave a card already in grave, $card.")
    }

    // TODO: emit an event for sent to grave
    card.location = InGraveyard
    card.maybeControlledState = None // clear it out if it was present
    graveyard.append(card)
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

sealed trait InExtraDeck extends InFieldZone
object InExtraDeck extends InExtraDeck

sealed trait InPendulumZone extends InFieldZone
case object InLeftPendulumZone extends InPendulumZone
case object InRightPendulumZone extends InPendulumZone

object Location {
  val MonsterZones = Set(InMonster0, InMonster1, InMonster2, InMonster3, InMonster4)
  val SpellTrapZones = Set(InSpellTrap0, InSpellTrap1, InSpellTrap2, InSpellTrap3, InSpellTrap4)
  val PendulumZones = Set(InLeftPendulumZone, InRightPendulumZone)

  val FieldZones = Seq(MonsterZones, SpellTrapZones, Set(InFieldSpell), PendulumZones).map(_.asInstanceOf[Set[InFieldZone]]).reduce(_ & _)

  val Locations = FieldZones.asInstanceOf[Set[Location]] & Set(InDeck, InHand, InGraveyard, InBanished)
}
