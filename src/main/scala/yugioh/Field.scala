package yugioh

import yugioh.action.{Action, ActionModule}
import yugioh.card.monster.{Monster, PendulumMonster, Position}
import yugioh.card.spell.FieldSpell
import yugioh.card.state._
import yugioh.card.{Card, SpellOrTrap}
import yugioh.events.EventsModuleComponent

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

  def placeAsMonster(monster: Monster, position: Position, howSummoned: HowSummoned, positionPreference: Option[Int] = None): InMonsterZone
  def placeAsSpellOrTrap(spellOrTrap: SpellOrTrap, faceup: Boolean, positionPreference: Option[Int] = None): InSpellTrapZone

  def destroy(card: Card): Unit = sendToGrave(card) // TODO: must consider Bottomless Trap Hole

  def discard(card: Card): Unit = sendToGrave(card)

  def sendToGrave(card: Card): Unit

  /**
    * All the actions associated with a field, which includes grave and banished; may be empty.
    */
  def actions(implicit gameState: GameState, actionModule: ActionModule): Seq[Action]
}

trait FieldModule {
  def createField: Field
}

trait FieldModuleComponent {
  def fieldModule: FieldModule
}

trait DefaultFieldModuleComponent extends FieldModuleComponent {
  self: EventsModuleComponent =>

  def fieldModule = new FieldModule {
    override def createField = new Field {
      override def placeAsMonster(monster: Monster, position: Position, howSummoned: HowSummoned, locationPreference: Option[Int]) = {
        monster.maybeControlledState = Some(MonsterControlledState(position))
        monster.maybeMonsterFieldState = Some(MonsterFieldState(howSummoned))
        placeAsHelper(monsterZones, InMonsterZone.MonsterZones)(monster, locationPreference)
      }

      override def placeAsSpellOrTrap(spellOrTrap: SpellOrTrap, faceup: Boolean, locationPreference: Option[Int]) = {
        spellOrTrap.maybeControlledState = Some(SpellTrapControlledState(faceup))
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
          case InHand => remove(card.Owner.hand, card)
          case InGraveyard => remove(card.Owner.field.graveyard, card)
          case InBanished => remove(card.Owner.field.banished, card)
          case monsterZone: InMonsterZone => removeFromMonsterZone(monsterZone)
          case spellTrapZone: InSpellTrapZone => removeFromSpellTrapZone(spellTrapZone)
          case InFieldSpell => fieldSpellZone = None
          case InLeftPendulumZone => leftPendulumZone = None
          case InRightPendulumZone => rightPendulumZone = None
          case InDeck =>
            card.Owner.deck.cards.remove(card.Owner.deck.cards.indexOf(card))
            card.Owner.deck.shuffle()
          case InExtraDeck =>
            card.Owner.extraDeck.remove(card.Owner.extraDeck.indexOf(card))
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

      override def actions(implicit gameState: GameState, actionModule: ActionModule) = {
        ((monsterZones ++ spellTrapZones :+ fieldSpellZone :+ leftPendulumZone :+ rightPendulumZone).flatten
          ++ graveyard ++ banished).flatMap(_.actions)
      }

      override def sendToGrave(card: Card): Unit = {
        // remove from current location
        card.location match {
          case InDeck =>
            card.Owner.deck.cards.remove(card.Owner.deck.cards.indexOf(card))
          case InHand =>
            card.Owner.hand.remove(card.Owner.hand.indexOf(card))
          case InBanished =>
            card.Owner.field.banished.remove(card.Owner.field.banished.indexOf(card))
          case InExtraDeck =>
            card.Owner.extraDeck.remove(card.Owner.extraDeck.indexOf(card))
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

        // give it field state if it didn't already have it and needs it
        card match {
          case monster: Monster if monster.maybeMonsterFieldState.isEmpty =>
            // not summoned is always correct, because it didn't already have state
            monster.maybeMonsterFieldState = Some(MonsterFieldState(NotSummoned))
          case _ => // ignore
        }

        // TODO: emit an event for sent to grave
        card.location = InGraveyard
        card.maybeControlledState = None // clear it out if it was present
        graveyard.append(card)
      }
    }
  }
}


sealed trait Location

/**
  * Makes the objects callable to see if a card has them as the location or not.
  */
sealed trait SimpleLocationChecker extends Location {
  def apply(card: Card): Boolean = card.location == this
}

case object InDeck extends SimpleLocationChecker
case object InHand extends SimpleLocationChecker
case object InGraveyard extends SimpleLocationChecker
case object InBanished extends SimpleLocationChecker

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
  def apply(location: Location): Boolean = MonsterZones.contains(location)
}

sealed trait InSpellTrapZone extends InFieldZone
case object InSpellTrap0 extends InSpellTrapZone
case object InSpellTrap1 extends InSpellTrapZone
case object InSpellTrap2 extends InSpellTrapZone
case object InSpellTrap3 extends InSpellTrapZone
case object InSpellTrap4 extends InSpellTrapZone

object InSpellTrapZone {
  val SpellTrapZones: Seq[InSpellTrapZone] = Seq(InSpellTrap0, InSpellTrap1, InSpellTrap2, InSpellTrap3, InSpellTrap4)
  def apply(card: Card): Boolean = SpellTrapZones.contains(card.location)
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
