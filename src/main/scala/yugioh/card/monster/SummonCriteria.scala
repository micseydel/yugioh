package yugioh.card.monster

/**
  * The criteria for a summon, such as material for a tribute, synchro or fusion summon.
  */
trait SummonCriteria {
  def validSelection(monsters: Seq[Monster]): Boolean
}

case class TributeSummonCriteria(count: Int) extends SummonCriteria {
  override def validSelection(monsters: Seq[Monster]): Boolean = {
    monsters.size == count
  }

  override def toString = {
    s"SummonCriteria $count monsters."
  }
}

case class SynchroSummonCriteria() extends SummonCriteria { // TODO
  override def validSelection(monsters: Seq[Monster]): Boolean = ???
}

case class XyzSummonCriteria() extends SummonCriteria { // TODO
override def validSelection(monsters: Seq[Monster]): Boolean = ???
}
