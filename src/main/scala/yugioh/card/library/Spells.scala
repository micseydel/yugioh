package yugioh.card.library

import yugioh.card._
import yugioh.card.spell.{Spell, SpellEffect}
import yugioh.{GameState, Player}


// this just makes IDE navigation easier
object Spells

// TODO: DianKetoTheCureMaster
object DianKetoTheCureMaster extends InstantiableCard[DianKetoTheCureMaster]
class DianKetoTheCureMaster(val owner: Player) extends Spell {
  override val PrintedName = "Dian Keto the Cure Master"

  override val effects: List[Effect] = List(new SpellEffect {
    override val Card: Card = DianKetoTheCureMaster.this

    override val Resolution: Resolution = new Resolution {
      override def resolve()(implicit gameState: GameState): Unit = {
        Card.owner.lifePoints += 1000
      }
    }
  })
}
