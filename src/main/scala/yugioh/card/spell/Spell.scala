package yugioh.card.spell

import sun.reflect.generics.reflectiveObjects.NotImplementedException
import yugioh._
import yugioh.action.CardActivation
import yugioh.card.state.SpellTrapControlledState
import yugioh.card._

trait Spell extends SpellOrTrap {
  private[this] val Owner = owner // for pattern matching

  override def actions(implicit gameState: GameState) = {
    if (effects.size == 1) {
      gameState match {
        case GameState(_, TurnPlayers(Owner, _), OpenGameState, MainPhase | MainPhase2, _, _) if canActivate =>
          Seq(CardActivation(this, Some(effects.head)))
        case _ => Seq()
      }
    } else {
      throw new NotImplementedException()
    }
  }

  private def canActivate(implicit gameState: GameState): Boolean = {
    // also assumes a single effect
    InSpellTrapZone(this) || (InHand(this) && controller.field.hasFreeSpellOrTrapZone) && effects.head.Conditions.met
  }
}

trait SpellEffect extends Effect {
  override val Conditions: Conditions = new Conditions {
    /**
      * By default, the condition for a spell is that the owner is the turn player and we're in open game state of a main phase.
      */
    override def met(implicit gameState: GameState): Boolean = {
      gameState match {
        case GameState(_, TurnPlayers(Card.owner, _), OpenGameState, MainPhase | MainPhase2, _, _) => true
        case _ => false
      }
    }
  }

  /**
    * By default, not much happens on activation (the chain is built elsewhere).
    */
  override val Activation = new Activation {
    override def activate()(implicit gameState: GameState): Unit = {
      Card.location match {
        case InHand =>
          Card.controller.field.placeAsSpellOrTrap(Card.asInstanceOf[SpellOrTrap], faceup = true)
        case _: InSpellTrapZone =>
          Card.maybeControlledState.get.asInstanceOf[SpellTrapControlledState].faceup = true
        case _ =>
          throw new IllegalStateException("A regular spell should not be activated from anywhere except the hand or set Spell/Trap zones.")
      }

      if (doesTarget) {
        selectedTargets = Card.controller.selectEffectTargets(availableTargets, targetCriteria)
      }
    }
  }
}

trait FieldSpell extends Spell
