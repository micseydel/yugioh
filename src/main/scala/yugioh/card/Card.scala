package yugioh.card

import sun.reflect.generics.reflectiveObjects.NotImplementedException
import yugioh._
import yugioh.action.{Action, ActionModule, CardActivation}
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
      case GameState(_, TurnPlayers(Owner, _), OpenGameState, _: MainPhase, _, _) if InHand(this) =>
        Seq(actionModule.newSetAsSpellOrTrap(this))
      case _ => Seq()
    }
  }
}

/**
  * Should not be inherited from outside of game mechanics. Instead, use one of:
  * NormalTrap, ContinuousTrap, CounterTrap.
  */
sealed trait NonContinuousSpellOrTrap extends SpellOrTrap {
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

// spells

sealed trait Spell extends SpellOrTrap {
  override def actions(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Seq[Action] = {
    val maybeActivation = if (Effects.size == 1) {
      gameState match {
        case GameState(_, TurnPlayers(Owner, _), OpenGameState, _: MainPhase, _, _) if canActivate =>
          Seq(CardActivation(this, Owner))
        case _ => Seq()
      }
    } else {
      throw new NotImplementedException()
    }

    // super allows setting
    super.actions ++ maybeActivation
  }

  private def canActivate(implicit gameState: GameState): Boolean = {
    // also assumes a single effect
    assert(Effects.length == 1, "Code assumes only a single effect will be present.")

    // if activation condition is met, and the spell is either already on the field or it's in hand and there's space to place it...
    Effects.head.ActivationConditions.met && (InSpellTrapZone(this) || (InHand(this) && controller.field.hasFreeSpellOrTrapZone))
  }
}

trait NormalSpell extends Spell with NonContinuousSpellOrTrap
trait QuickPlaySpell extends Spell with NonContinuousSpellOrTrap
trait RitualSpell extends Spell with NonContinuousSpellOrTrap
trait EquipSpell extends Spell
trait ContinuousSpell extends Spell
trait FieldSpell extends Spell

trait SpellEffect extends Effect {
  override def activationTimingCorrect(implicit gameState: GameState): Boolean = {
    gameState match {
      case GameState(_, TurnPlayers(Card.Owner, _), OpenGameState, _: MainPhase, _, _) => true
      case _ => false
    }
  }
}

// traps

sealed trait Trap extends SpellOrTrap

trait NormalTrap extends Trap with NonContinuousSpellOrTrap {
  override def actions(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Seq[Action] = {
    val maybeActivation = Effects match {
      case Seq(effect) if canActivate && effect.activationTimingCorrect =>
        Seq(CardActivation(this, Owner))
      case _ =>
        Seq() // subclass will have to write specific logic for this
    }

    // super handles setting
    super.actions ++ maybeActivation
  }

  private def canActivate(implicit gameState: GameState): Boolean = {
    // if activation condition is met, and the spell is already set on the field, and it wasn't set this turn...
    Effects.head.ActivationConditions.met && InSpellTrapZone(this) && maybeControlledState.map(!_.faceup).get && maybeTurnSet.exists(_ < gameState.turnCount)
  }
}

trait CounterTrap extends Trap with NonContinuousSpellOrTrap
trait ContinuousTrap extends Trap

trait TrapEffect extends Effect {
  /**
    * Applicable for non-counter traps.
    */
  override def activationTimingCorrect(implicit gameState: GameState): Boolean = {
    gameState match {
      case GameState(_, _, _, _, _: DamageStep, _) =>
        false
      case GameState(_, _, _: CheckForTrigger | _: PlayerFastEffects | _: ChainRules, _, _, _) =>
        true
      case _ =>
        false
    }
  }
}
