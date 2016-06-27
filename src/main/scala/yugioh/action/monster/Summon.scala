package yugioh.action.monster

import yugioh._
import yugioh.action.InherentAction
import yugioh.card.monster.{Attack, Monster, Set, TributeSummonCriteria}
import yugioh.card.state._


trait Summon extends InherentAction {
  val monster: Monster
  val player = monster.owner

  override def toString = s"${this.getClass.getSimpleName}($monster)"
}

trait NormalSummon extends Summon

class NormalSummonImpl(val monster: Monster) extends NormalSummon {
  override protected def doAction()(implicit gameState: GameState) = {
    monster.owner.field.placeAsMonster(monster, Attack, NormalSummoned)
    monster.maybeMonsterControlledState.get.manuallyChangedPositionsThisTurn = true // TODO: consolidate
  }
}

trait TributeSummon extends NormalSummon

class TributeSummonImpl(override val monster: Monster) extends NormalSummonImpl(monster) with TributeSummon {
  override protected def doAction()(implicit gameState: GameState) = {
    val summonCriteria = TributeSummonCriteria(if (monster.maybeLevel.get < 7) 1 else 2)
    val toTribute = monster.owner.selectSummonMaterial(monster, summonCriteria, monster.owner.field.monsterZones.toSeq.flatten)
    for (tribute <- toTribute) {
      tribute.sendToGrave()
      // TODO: (create and) emit an event for UsedForTributeSummon or something
    }

    // TODO LOW: cleanup this kludge where useless operations are done
    super.doAction()
    monster.maybeMonsterFieldState.get.howSummoned = TributeSummoned
  }
}

trait FlipSummon extends Summon with SwitchPosition

class FlipSummonImpl(override val monster: Monster) extends FlipSummon {
  override protected def doAction()(implicit gameState: GameState) = {
    for (state <- monster.maybeMonsterControlledState) {
      state.position = Attack
      state.manuallyChangedPositionsThisTurn = true
    }
  }
}

trait SetAsMonster extends InherentAction {
  val monster: Monster
  val player = monster.owner
  override def toString = s"${this.getClass.getSimpleName}($monster)"
}

class SetAsMonsterImpl(override val monster: Monster) extends SetAsMonster {
  override protected def doAction()(implicit gameState: GameState) = {
    monster.owner.field.placeAsMonster(monster, Set, NotSummoned)
    monster.maybeControlledState = Some(new MonsterControlledState(Set))
    monster.maybeMonsterControlledState.get.manuallyChangedPositionsThisTurn = true // TODO: consolidate
  }
}

trait TributeSet extends SetAsMonster

class TributeSetImpl(monster: Monster) extends SetAsMonsterImpl(monster) with TributeSet {
  override protected def doAction()(implicit gameState: GameState) = {
    val summonCriteria = TributeSummonCriteria(if (monster.maybeLevel.get < 7) 1 else 2)
    val toTribute = monster.owner.selectSummonMaterial(monster, summonCriteria, monster.owner.field.monsterZones.toSeq.flatten)
    for (tribute <- toTribute) {
      tribute.sendToGrave()
    }

    super.doAction()
  }
}
