package yugioh.card.monster

import yugioh.{Criteria, GameState, Player}

/**
  * The criteria for a summon, such as material for a tribute, synchro or fusion summon.
  */
trait SummonCriteria extends Criteria[Monster] {
  val monster: Monster
}

object TributeSummonCriteria {
  def apply(player: Player, monster: Monster): TributeSummonCriteria = {
    new TributeSummonCriteria(player, monster)
  }
}

case class TributeSummonCriteria(player: Player, monster: Monster, requiredTributes: Int) extends SummonCriteria {
  def this(player: Player, monster: Monster) = {
    this(player, monster, if (monster.maybeLevel.get < 7) 1 else 2)
  }

  override def toString = s"$requiredTributes monsters"

  // TODO LOW: tribute summon requirements are only meetable if there are a sufficient number of monsters on the field that can be used for a tribute summon
  override def meetable(implicit gameState: GameState): Boolean = availableChoices.size >= requiredTributes

  override def availableChoices(implicit gameState: GameState): Seq[Monster] = {
    player.field.monsterZones.filter(_.nonEmpty).flatten.toSeq
  }

  override def validSelection[T >: Monster](choices: Seq[T])(implicit gameState: GameState): Boolean = choices.size == requiredTributes
}
