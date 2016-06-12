package yugioh.action.monster

import yugioh._
import yugioh.action.InherentAction
import yugioh.card.monster.{Attack, Monster, Set}
import yugioh.card.state._


trait Summon extends InherentAction {
  val monster: Monster

  override def toString = s"${this.getClass.getSimpleName}($monster)"
}

trait NormalSummon extends Summon

class NormalSummonImpl(val monster: Monster) extends NormalSummon {
  override protected def doAction()(implicit gameState: GameState) = {
    monster.owner.field.placeAsMonster(monster)
    monster.maybeControlledState = Some(new MonsterControlledState(Attack))
    monster.maybeMonsterControlledState.get.manuallyChangedPositionsThisTurn = true // TODO: consolidate
    monster.maybeMonsterFieldState = Some(new MonsterFieldStateImpl(NormalSummoned))
  }
}

trait TributeSummon extends NormalSummon

class TributeSummonImpl(override val monster: Monster) extends NormalSummonImpl(monster) with TributeSummon {
  override protected def doAction()(implicit gameState: GameState) = {
    val toTribute = monster.owner.selectSummonMaterial(monster, monster.owner.field.monsterZones.toSeq.flatten)
    for (tribute <- toTribute) {
      tribute.owner.field.sendToGrave(tribute)
      // TODO: (create and) emit an event for UsedForTributeSummon or something
    }

    // TODO LOW: cleanup this kludge where useless operations are done
    super.doAction()
    monster.maybeMonsterControlledState.get.position = Attack
    monster.maybeMonsterFieldState = Some(new MonsterFieldStateImpl(TributeSummoned))
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
  override def toString = s"${this.getClass.getSimpleName}($monster)"
}

class SetAsMonsterImpl(override val monster: Monster) extends SetAsMonster {
  override protected def doAction()(implicit gameState: GameState) = {
    monster.owner.field.placeAsMonster(monster)
    monster.maybeControlledState = Some(new MonsterControlledState(Set))
    monster.maybeMonsterControlledState.get.manuallyChangedPositionsThisTurn = true // TODO: consolidate
    monster.maybeMonsterFieldState = Some(new MonsterFieldStateImpl(NotSummoned))
  }
}

trait TributeSet extends SetAsMonster

class TributeSetImpl(monster: Monster) extends SetAsMonsterImpl(monster) with TributeSet {
  override protected def doAction()(implicit gameState: GameState) = {
    val toTribute = monster.owner.selectSummonMaterial(monster, monster.owner.field.monsterZones.toSeq.flatten)
    for (tribute <- toTribute) {
      tribute.owner.field.sendToGrave(tribute)
    }

    super.doAction()
  }
}
