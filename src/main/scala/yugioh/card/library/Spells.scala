package yugioh.card.library

import yugioh.action._
import yugioh.action.monster.SpecialSummonImpl
import yugioh.card._
import yugioh.card.monster.{Attack, Defense, Monster}
import yugioh.card.spell.{Spell, SpellEffect}
import yugioh.events.EventsModule
import yugioh.{Criteria, GameState, InGraveyard, Player}


// this just makes IDE navigation easier
private object Spells

object CardDestruction extends InstantiableCard[CardDestruction]
class CardDestruction(val Owner: Player) extends Spell {
  override val PrintedName = "Card Destruction"

  override val effects: List[Effect] = List(new CardDestructionEffect)

  class CardDestructionEffect extends SpellEffect {
    override val Card: Card = CardDestruction.this

    override val Conditions = new Conditions {
      override def met(implicit gameState: GameState): Boolean = {
        // cannot activate if not enough cards in the deck, and there must be at least one card in at least one hand
        controller.deck.remaining >= controller.hand.size &&
          gameState.turnPlayers.both.exists(_.hand.nonEmpty)
      }
    }

    override val Resolution = new Resolution {
      override val Effect: Effect = CardDestructionEffect.this

      case class DiscardBothHands(parent: Action, existsInAChainAction: ExistsInAChainAction) extends InherentAction {
        override val maybeParent = Some(existsInAChainAction)

        override val player: Player = controller

        override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule) = {
          for (player <- gameState.turnPlayers.both) {
            new DiscardImpl(controller, player.hand, existsInAChainAction).execute() // TODO: decouple
          }
        }
      }

      class BothDraw(handSizes: Seq[(Player, Int)], existsInAChainAction: ExistsInAChainAction) extends InherentAction {
        override val maybeParent = Some(existsInAChainAction)

        override val player: Player = controller

        override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule) = {
          for ((player, handSize) <- handSizes) {
            new DrawImpl(player, handSize, existsInAChainAction).execute() // TODO: decouple
          }
        }
      }

      override def resolve(existsInAChainAction: ExistsInAChainAction)(implicit gameState: GameState, eventsModule: EventsModule) = {
        val handSizes = gameState.turnPlayers.both.map(player => (player, player.hand.size))

        new DiscardBothHands(existsInAChainAction, existsInAChainAction).execute()

        new BothDraw(handSizes, existsInAChainAction).execute()
      }
    }
  }
}

object DarkHole extends InstantiableCard[DarkHole]
class DarkHole(val Owner: Player) extends Spell {
  override val PrintedName = "Dark Hole"

  override val effects: List[Effect] = List(new DarkHoleEffect)

  class DarkHoleEffect extends SpellEffect {
    override val Card: Card = DarkHole.this

    override val Conditions = new Conditions {
      override def met(implicit gameState: GameState): Boolean = {
        gameState.turnPlayers.both.flatMap(_.field.monsterZones).flatten.nonEmpty
      }
    }

    override val Resolution = new Resolution {
      val Effect = DarkHoleEffect.this

      override def resolve(existsInAChainAction: ExistsInAChainAction)(implicit gameState: GameState, eventsModule: EventsModule) = {
        new DestroyImpl(Owner, gameState.turnPlayers.both.flatMap(_.field.monsterZones).flatten, existsInAChainAction).execute() // TODO: decouple
      }
    }
  }
}

object DianKetoTheCureMaster extends InstantiableCard[DianKetoTheCureMaster]
class DianKetoTheCureMaster(val Owner: Player) extends Spell {
  override val PrintedName = "Dian Keto the Cure Master"

  override val effects: List[Effect] = List(new DianKetoTheCureMasterEffect)

  class DianKetoTheCureMasterEffect extends SpellEffect {
    override val Card: Card = DianKetoTheCureMaster.this

    override val Resolution = new Resolution {
      val Effect = DianKetoTheCureMasterEffect.this

      override def resolve(existsInAChainAction: ExistsInAChainAction)(implicit gameState: GameState, eventsModule: EventsModule) = {
        Card.Owner.lifePoints += 1000 // TODO LOW: this should be refactored out into an action
      }
    }
  }
}

object MonsterReborn extends InstantiableCard[MonsterReborn]
class MonsterReborn(val Owner: Player) extends Spell {
  override val PrintedName = "Monster Reborn"

  override val effects: List[Effect] = List(new MonsterRebornEffect)

  class MonsterRebornEffect extends SpellEffect {
    override val Card: Card = MonsterReborn.this

    override def targetCriteria[C <: Card](implicit gameState: GameState) = new Criteria[C] {
      override def validSelection(choices: Seq[C]): Boolean = {
        choices.size == 1 && InGraveyard(choices.head)
      }
    }

    /**
      * Properly summoned monsters in either player's graveyard.
      */
    override def availableTargets(implicit gameState: GameState) = {
      gameState.turnPlayers.both
        .flatMap(_.field.graveyard)
        .filter(_.isInstanceOf[Monster])
        .map(_.asInstanceOf[Monster])
        .filter(monster => monster.maybeMonsterFieldState.get.properlySummoned(monster)) // TODO LOW: monster is redundant here
    }

    override val Resolution = new Resolution {
      override val Effect: Effect = MonsterRebornEffect.this

      override def resolve(existsInAChainAction: ExistsInAChainAction)(implicit gameState: GameState, eventsModule: EventsModule) = {
        val target = selectedTargets.head.asInstanceOf[Monster]

        if (InGraveyard(target) && controller.field.hasFreeMonsterZone) {
          val position = Owner.selectSpecialSummonPosition(target, Seq(Attack, Defense))
          SpecialSummonImpl(controller, target, position, existsInAChainAction).execute() // TODO: decouple from implementation somehow
        } // else fizzle
      }
    }

    override val Conditions = new Conditions {
      override def met(implicit gameState: GameState): Boolean = {
        availableTargets.nonEmpty && controller.field.hasFreeMonsterZone
      }
    }
  }
}
