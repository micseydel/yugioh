package yugioh

import yugioh.Util.remove
import yugioh.action.card.CardMovedAction
import yugioh.action.{Action, ActionModule, Cause}
import yugioh.card.Card.AnyCard
import yugioh.card.monster.{Monster, PendulumMonster, Position}
import yugioh.card.state._
import yugioh.card.{FieldSpell, SpellOrTrap}
import yugioh.events.{EventsModule, EventsModuleComponent}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Note that the methods here which have side effects, such as placing, destroying, discarding and sending to grave,
  * should all only ever be called from an Action which will emit an event when it occurs.
  */
trait Field {
  // try to put things toward the center first
  protected val PositionPriorities = Seq(2, 1, 3, 0, 4)

  val monsterZones: Array[Option[Monster]] = Array.fill(5)(None)
  val spellTrapZones: Array[Option[SpellOrTrap]] = Array.fill(5)(None)

  val graveyard = new ListBuffer[AnyCard]
  val banished = new ListBuffer[AnyCard]

  var fieldSpellZone: Option[FieldSpell] = None

  var leftPendulumZone: Option[PendulumMonster] = None
  var rightPendulumZone: Option[PendulumMonster] = None

  def hasFreeMonsterZone: Boolean = monsterZones.exists(_.isEmpty)

  def hasFreeSpellOrTrapZone: Boolean = spellTrapZones.exists(_.isEmpty)

  def placeAsMonster(cause: Cause, monster: Monster, position: Position, howSummoned: HowSummoned, positionPreference: Option[Int] = None)(implicit gameState: GameState, actionModule: ActionModule): InMonsterZone

  def placeAsSpellOrTrap(cause: Cause, spellOrTrap: SpellOrTrap, faceup: Boolean, positionPreference: Option[Int] = None)(implicit gameState: GameState, actionModule: ActionModule): InSpellTrapZone

  def destroy(cause: Cause, card: AnyCard)(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = sendToGrave(cause, card) // TODO LOW: must consider Bottomless Trap Hole

  def discard(cause: Cause, card: AnyCard)(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = sendToGrave(cause, card)

  def sendToGrave(cause: Cause, card: AnyCard)(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit

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

  implicit def fieldModule: FieldModule = new FieldModule {
    override def createField: Field = new Field {
      override def placeAsMonster(cause: Cause, monster: Monster, position: Position, howSummoned: HowSummoned, locationPreference: Option[Int])(implicit gameState: GameState, actionModule: ActionModule): InMonsterZone = {
        monster.maybeControlledState = Some(MonsterControlledState(position))
        monster.maybeMonsterFieldState = Some(MonsterFieldState(monster, howSummoned))
        placeAsHelper(monsterZones, InMonsterZone.MonsterZones)(cause, monster, locationPreference)
      }

      override def placeAsSpellOrTrap(cause: Cause, spellOrTrap: SpellOrTrap, faceup: Boolean, locationPreference: Option[Int])(implicit gameState: GameState, actionModule: ActionModule): InSpellTrapZone = {
        spellOrTrap.maybeControlledState = Some(SpellOrTrapControlledState(faceup))
        placeAsHelper(spellTrapZones, InSpellTrapZone.SpellTrapZones)(cause, spellOrTrap, locationPreference)
      }

      private def placeAsHelper[C <: AnyCard, Z <: InFieldZone](destinationArray: Array[Option[C]], destinationLocation: Seq[Z])
                                                               (cause: Cause, card: C, locationPreference: Option[Int])(implicit gameState: GameState, actionModule: ActionModule): Z = {
        val position = locationPreference.getOrElse(PositionPriorities.filter(destinationArray(_).isEmpty).head)
        destinationArray.update(position, Some(card))

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
        CardMovedAction(cause, card, card.location, toWhere).execute()
        toWhere
      }

      def removeFromMonsterZone[Z <: InMonsterZone](monsterZone: Z): Unit = {
        removeFromHelper(monsterZones, monsterZone, InMonsterZone.MonsterZones)
      }

      def removeFromSpellTrapZone[Z <: InSpellTrapZone](spellTrapZone: Z): Unit = {
        removeFromHelper(spellTrapZones, spellTrapZone, InSpellTrapZone.SpellTrapZones)
      }

      private def removeFromHelper[T](removeFrom: Array[Option[T]], zone: InFieldZone, zones: Seq[InFieldZone]): Unit = {
        removeFrom.update(zones.indexOf(zone), None)
      }

      override def actions(implicit gameState: GameState, actionModule: ActionModule): mutable.ArraySeq[Action] = {
        ((monsterZones ++ spellTrapZones :+ fieldSpellZone :+ leftPendulumZone :+ rightPendulumZone).flatten
          ++ graveyard ++ banished).flatMap(_.actions)
      }

      override def sendToGrave(cause: Cause, card: AnyCard)(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
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
            monster.maybeMonsterFieldState = Some(MonsterFieldState(monster, NotSummoned))
          case _ => // ignore
        }

        CardMovedAction(cause, card, card.location, InGraveyard).execute()
        graveyard.append(card)
      }

      // TODO: return to hand; should remove the controlled state
    }
  }
}


sealed trait Location

/**
  * Makes the objects callable to see if a card has them as the location or not.
  */
sealed trait SimpleLocationChecker extends Location {
  def apply(card: AnyCard): Boolean = card.location == this
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

  def apply(card: SpellOrTrap): Boolean = SpellTrapZones.contains(card.location)
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

  // TODO LOW: can probably get rid of the asInstanceOf call
  val FieldZones: Set[Location] = Seq(MonsterZones, SpellTrapZones, Set(InFieldSpell), PendulumZones).map(_.asInstanceOf[Set[Location]]).reduce(_ & _)

  val Locations: Set[Location] = FieldZones.asInstanceOf[Set[Location]] & Set(InDeck, InHand, InGraveyard, InBanished)
}
