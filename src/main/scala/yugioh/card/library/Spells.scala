package yugioh.card.library

import yugioh.action._
import yugioh.card._
import yugioh.card.monster.{Attack, Defense, Monster}
import yugioh.card.spell.{NormalSpell, SpellEffect}
import yugioh.events.EventsModule
import yugioh.{Criteria, GameState, InGraveyard, Player}


// this just makes IDE navigation easier
private object Spells

object CardDestruction extends InstantiableCard[CardDestruction]
class CardDestruction(val Owner: Player) extends NormalSpell {
  override val PrintedName = "Card Destruction"

  override val Effects: List[Effect] = List(CardDestructionEffect)

  object CardDestructionEffect extends SpellEffect {
    override lazy val Card: Card = CardDestruction.this
    override val EffectType: EffectType = Effect
    override val Cost: InherentAction = NoAction(Card.Owner)
    override val maybeTargetCriteria = None
    override val maybeCostCriteria = None

    override val Resolution = new InherentAction {
      override val player: Player = controller

      case class DiscardBothHands() extends InherentAction {
        override val player: Player = controller
        override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
          for (player <- gameState.turnPlayers.both) {
            actionModule.newDiscard(controller, player.hand).execute()
          }
        }
      }

      case class BothDraw(handSizes: Seq[(Player, Int)]) extends InherentAction {
        override val player: Player = controller
        override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
          for ((player, handSize) <- handSizes) {
            actionModule.newDraw(player, handSize).execute()
          }
        }
      }

      override def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
        val handSizes = gameState.turnPlayers.both.map(player => (player, player.hand.size))

        DiscardBothHands().andThen(BothDraw(handSizes)).execute()
      }
    }

    override def specialActivationConditionsMet(implicit gameState: GameState): Option[Boolean] = {
      Some(controller.deck.remaining >= controller.hand.size &&
        gameState.turnPlayers.both.exists(_.hand.nonEmpty))
    }
  }
}

object DarkHole extends InstantiableCard[DarkHole]
class DarkHole(val Owner: Player) extends NormalSpell {
  override val PrintedName = "Dark Hole"

  override val Effects: List[Effect] = List(new DarkHoleEffect)

  class DarkHoleEffect extends SpellEffect {
    override val Card: Card = DarkHole.this

    override val Resolution = new InherentAction {
      override val player: Player = Owner
      override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
        actionModule.newDestroy(Owner, gameState.turnPlayers.both.flatMap(_.field.monsterZones).flatten)
      }
    }

    override val EffectType: EffectType = Effect
    override val Cost: InherentAction = NoAction(Card.Owner)
    override val maybeTargetCriteria = None
    override val maybeCostCriteria = None

    override def specialActivationConditionsMet(implicit gameState: GameState): Option[Boolean] = {
      Some(gameState.turnPlayers.both.flatMap(_.field.monsterZones).flatten.nonEmpty)
    }
  }
}

object DianKetoTheCureMaster extends InstantiableCard[DianKetoTheCureMaster]
class DianKetoTheCureMaster(val Owner: Player) extends NormalSpell {
  override val PrintedName = "Dian Keto the Cure Master"

  override val Effects: List[Effect] = List(new DianKetoTheCureMasterEffect)

  class DianKetoTheCureMasterEffect extends SpellEffect {
    override val Card: Card = DianKetoTheCureMaster.this

    override val Resolution = new InherentAction {
      override val player: Player = Card.Owner
      override def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
        actionModule.newChangeLifePoints(1000, player).execute()
      }
    }

    override val EffectType: EffectType = Effect
    override val Cost: InherentAction = NoAction(Card.Owner)
    override val maybeTargetCriteria = None
    override val maybeCostCriteria = None
  }
}

object MonsterReborn extends InstantiableCard[MonsterReborn]
class MonsterReborn(val Owner: Player) extends NormalSpell {
  override val PrintedName = "Monster Reborn"

  override val Effects: List[Effect] = List(new MonsterRebornEffect)

  class MonsterRebornEffect extends SpellEffect {
    override val Card: Card = MonsterReborn.this

    override val Resolution = new InherentAction {
      override def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
        val target = selectedTargets.head.asInstanceOf[Monster]

        if (InGraveyard(target) && controller.field.hasFreeMonsterZone) {
          val position = Owner.selectSpecialSummonPosition(target, Seq(Attack, Defense))
          actionModule.newSpecialSummon(controller, target, position).execute()
        } // else fizzle
      }

      override val player: Player = Card.Owner
    }

    override val EffectType: EffectType = Effect
    override val Cost: InherentAction = NoAction(Card.Owner)
    override val maybeCostCriteria = None

    override val maybeTargetCriteria = Some(new Criteria[Card] {
      override def meetable(implicit gameState: GameState): Boolean = availableChoices.nonEmpty

      override def validSelection(choices: Seq[Card])(implicit gameState: GameState): Boolean = choices.size == 1

      override def availableChoices(implicit gameState: GameState): Seq[Card] = {
        gameState.turnPlayers.both
          .flatMap(_.field.graveyard)
          .filter(_.isInstanceOf[Monster])
          .map(_.asInstanceOf[Monster])
          .filter(monster => monster.maybeMonsterFieldState.get.properlySummoned(monster)) // TODO LOW: cleanup (monster is redundant here)
      }
    })
  }
}
