package yugioh.card.library

import yugioh.action.monster.SpecialSummonImpl
import yugioh.card._
import yugioh.card.monster.{Attack, Defense, Monster}
import yugioh.card.spell.{Spell, SpellEffect}
import yugioh.{Criteria, GameState, InGraveyard, Player}


// this just makes IDE navigation easier
private object Spells

object DarkHole extends InstantiableCard[DarkHole]
class DarkHole(val Owner: Player) extends Spell {
  override val PrintedName = "Dark Hole"

  override val effects: List[Effect] = List(new SpellEffect {
    override val Card: Card = DarkHole.this

    override val Conditions = new Conditions {
      override def met(implicit gameState: GameState): Boolean = {
        gameState.turnPlayers.both.flatMap(_.field.monsterZones).flatten.nonEmpty
      }
    }

    override val Resolution = new Resolution {
      override def resolve()(implicit gameState: GameState): Unit = {
        for (monster <- gameState.turnPlayers.both.flatMap(_.field.monsterZones).flatten) {
          monster.destroy()
        }
      }
    }
  })
}

object DianKetoTheCureMaster extends InstantiableCard[DianKetoTheCureMaster]
class DianKetoTheCureMaster(val Owner: Player) extends Spell {
  override val PrintedName = "Dian Keto the Cure Master"

  override val effects: List[Effect] = List(new SpellEffect {
    override val Card: Card = DianKetoTheCureMaster.this

    override val Resolution = new Resolution {
      override def resolve()(implicit gameState: GameState): Unit = {
        Card.Owner.lifePoints += 1000
      }
    }
  })
}

object MonsterReborn extends InstantiableCard[MonsterReborn]
class MonsterReborn(val Owner: Player) extends Spell {
  override val PrintedName = "Monster Reborn"

  override val effects: List[Effect] = List(new SpellEffect {
    override val Card: Card = MonsterReborn.this

    override def targetCriteria[C <: Card](implicit gameState: GameState) = new Criteria[C] {
      override def validSelection(choices: Seq[C]): Boolean = {
        choices.size == 1 && choices.head.location == InGraveyard
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
        .filter(monster => monster.maybeMonsterFieldState.get.properlySummoned(monster))
    }

    override val Resolution = new Resolution {
      override def resolve()(implicit gameState: GameState): Unit = {
        val target = selectedTargets.head.asInstanceOf[Monster]

        val position = Owner.selectSpecialSummonPosition(target, Seq(Attack, Defense))
        if (target.location == InGraveyard && controller.field.hasFreeMonsterZone) {
          SpecialSummonImpl(controller, target, position).execute() // TODO: decouple from implementation somehow
        } // else fizzle
      }
    }

    override val Conditions = new Conditions {
      override def met(implicit gameState: GameState): Boolean = {
        availableTargets.nonEmpty && controller.field.hasFreeMonsterZone
      }
    }
  })
}
