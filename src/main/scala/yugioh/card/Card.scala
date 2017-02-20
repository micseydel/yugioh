package yugioh.card

import yugioh._
import yugioh.action.{Action, ActionModule, SetCard}
import yugioh.card.monster.Monster
import yugioh.card.state.{ControlledState, SpellOrTrapControlledState}
import yugioh.events.EventsModule

object Card {
  type AnyCard = Card[_ <: ControlledState]
}

trait Card[CS <: ControlledState] {
  val PrintedName: String
  val Owner: Player
  var location: Location = InDeck

  var controller: Player = Owner
  def actions(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Seq[Action]

  private[this] var _maybeControlledState: Option[CS] = None

  def maybeControlledState: Option[CS] = _maybeControlledState

  def maybeControlledState_=(controlledState: Option[CS]): Unit = {
    for (controlledState <- maybeControlledState) {
      controlledState.close()
    }

    _maybeControlledState = controlledState
  }

  def name: String = PrintedName

  override def toString: String = name

  def toString(viewer: Player): String = {
    maybeControlledState.map { controlledState =>
      if (controlledState.faceup) {
        name
      } else {
        if (viewer == Owner) {
          s"Set($this)"
        } else {
          "<Set>"
        }
      }
    }.getOrElse(name)
  }

  def discard()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = Owner.field.discard(this)
  def destroy()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = Owner.field.destroy(this)
  def sendToGrave()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = Owner.field.sendToGrave(this)

  /**
    * Allows the card to be notified of when it's moved.
    */
  def notifyMoved()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = ()
}

object EffectCard {
  type AnyEffectCard = EffectCard[_ <: ControlledState]
}

trait EffectCard[CS <: ControlledState] extends Card[CS] {
  var activated = false

  val Effects: Seq[Effect]
}

trait SpellOrTrap extends EffectCard[SpellOrTrapControlledState] { // TODO: spell speed!
  /**
    * The turn this card was set on the field, if at all.
    */
  var maybeTurnSet: Option[Int] = None

  override def actions(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Seq[Action] = {
    gameState match {
      case GameState(_, TurnPlayers(Owner, _), OpenGameState, MainPhase | MainPhase2, _, _) if InHand(this) =>
        Seq(actionModule.newSetAsSpellOrTrap(this))
      case _ => Seq()
    }
  }
}

/**
  * Should not be inherited from outside of game mechanics. Instead, use one of:
  * NormalTrap, ContinuousTrap, CounterTrap.
  * TODO LOW: refactor to be `sealed` (non-trivial)
  */
trait NonContinuousSpellOrTrap extends SpellOrTrap {
  /**
    * After a chain has resolved that involved this card, and it remains on the field, send it to grave.
    */
  def afterChainCleanup()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    if (InSpellTrapZone(this)) {
      sendToGrave() // TODO: this should be an action for which game mechanics are responsible
    }
  }
}

trait ContinuousSpellOrTrap extends SpellOrTrap

// inherent action
trait SetAsSpellOrTrap extends SetCard {
  val spellOrTrap: SpellOrTrap
}

class SetAsSpellOrTrapImpl(override val spellOrTrap: SpellOrTrap) extends SetAsSpellOrTrap {
  override val player: Player = spellOrTrap.Owner

  override val toString = s"SetAsSpellOrTrap($spellOrTrap)"

  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    if (player.field.hasFreeSpellOrTrapZone) {
      player.field.placeAsSpellOrTrap(spellOrTrap, faceup = false)
      spellOrTrap.maybeTurnSet = Some(gameState.turnCount)
    } else {
      throw new IllegalStateException(s"Tried to set $spellOrTrap but there was no space.")
    }
  }
}

sealed trait Delegate

/**
  * This is a wrapper so that a Monster can be treated as a SpellOrTrap card.
  */
trait SpellOrTrapDelegate extends Delegate with SpellOrTrap

/**
  * This is essentially a wrapper that allows a SpellOrTrap to be treated as a Monster card.
  */
trait MonsterDelegate extends Delegate with Monster

sealed trait CardMoved
