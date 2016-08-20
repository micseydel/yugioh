package yugioh.card.monster

import yugioh.{Criteria, GameState, Player}

/**
  * The criteria for a summon, such as material for a tribute, synchro or fusion summon.
  */
trait SummonCriteria extends Criteria[Monster] {
  val monster: Monster
}

case class TributeSummonCriteria(player: Player, monster: Monster) extends Criteria[Monster] with SummonCriteria {
  private[this] val count = if (monster.maybeLevel.get < 7) 1 else 2

  override def toString = s"$count monsters"

  // TODO LOW: tribute summon requirements are only meetable if there are a sufficient number of monsters on the field that can be used for a tribute summon
  override def meetable(implicit gameState: GameState): Boolean = availableChoices.size >= count

  override def availableChoices(implicit gameState: GameState): Seq[Monster] = {
    player.field.monsterZones.filter(_.nonEmpty).flatten.toSeq
  }

  override def validSelection(choices: Seq[Monster])(implicit gameState: GameState): Boolean = choices.size == count
}
