package yugioh.card.monster

import yugioh.{CountCriteria, Criteria}

/**
  * The criteria for a summon, such as material for a tribute, synchro or fusion summon.
  */
trait SummonCriteria extends Criteria[Monster]

case class TributeSummonCriteria(count: Int) extends CountCriteria[Monster](count) with SummonCriteria {
  override def toString = s"$count monsters"
}

case class SynchroSummonCriteria() extends SummonCriteria { // TODO LOW
  override def validSelection(monsters: Seq[Monster]): Boolean = ???
}

case class XyzSummonCriteria() extends SummonCriteria { // TODO LOW
override def validSelection(monsters: Seq[Monster]): Boolean = ???
}
