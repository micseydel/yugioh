package yugioh.action

import yugioh._
import yugioh.card.monster.{Attack, Defense, Monster}
import yugioh.card.state._

trait MonsterAction

trait Summon extends InherentAction {
  val monster: Monster

  override def toString = s"${this.getClass.getSimpleName}($monster)"
}

trait NormalSummon extends Summon

class NormalSummonImpl(val monster: Monster) extends NormalSummon {
  override protected def doAction()(implicit gameState: GameState, turnPlayers: TurnPlayers, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step) = {
    monster.owner.field.placeAsMonster(monster)
    monster.maybeControlledState = Some(new MonsterControlledStateImpl(Attack))
    monster.maybeMonsterControlledState.get.manuallyChangedPositionsThisTurn = true // TODO: consolidate
    monster.maybeMonsterFieldState = Some(new MonsterFieldStateImpl(NormalSummoned))
  }
}

trait TributeSummon extends NormalSummon

class TributeSummonImpl(override val monster: Monster) extends NormalSummonImpl(monster) with TributeSummon {
  override protected def doAction()(implicit gameState: GameState, turnPlayers: TurnPlayers, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step) = {
    val toTribute = monster.owner.selectSummonMaterial(monster, monster.owner.field.monsterZones.toSeq.flatten)
    for (tribute <- toTribute) {
      tribute.owner.field.sendToGrave(tribute)
    }

    // TODO LOW: cleanup this kludge where useless operations are done
    super.doAction()
    monster.maybeMonsterControlledState.get.position = Attack
    monster.maybeMonsterFieldState = Some(new MonsterFieldStateImpl(TributeSummoned))
  }
}

trait SwitchPosition extends InherentAction {
  val monster: Monster
  override val toString = s"${this.getClass.getSimpleName}($monster(${monster.maybeMonsterControlledState.get.position}))"
}

class SwitchPositionImpl(override val monster: Monster) extends SwitchPosition {
  override protected def doAction()(implicit gameState: GameState, turnPlayers: TurnPlayers, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step) = {
    for (controlledState <- monster.maybeMonsterControlledState) {
      controlledState.manuallyChangedPositionsThisTurn = true
      controlledState.position match {
        case Attack => Defense
        case Defense => Attack
        case _ => throw new IllegalStateException("Shouldn't have tried to switch monster position.")
      }
    }
  }
}

trait FlipSummon extends Summon with SwitchPosition

class FlipSummonImpl(override val monster: Monster) extends FlipSummon {
  override protected def doAction()(implicit gameState: GameState, turnPlayers: TurnPlayers, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step) = {
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
  override protected def doAction()(implicit gameState: GameState, turnPlayers: TurnPlayers, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step) = {
    monster.owner.field.placeAsMonster(monster)
    monster.maybeControlledState = Some(new MonsterControlledStateImpl(yugioh.card.monster.Set))
    monster.maybeMonsterControlledState.get.manuallyChangedPositionsThisTurn = true // TODO: consolidate
    monster.maybeMonsterFieldState = Some(new MonsterFieldStateImpl(NotSummoned))
  }
}

trait TributeSet extends SetAsMonster

class TributeSetImpl(monster: Monster) extends SetAsMonsterImpl(monster) with TributeSet {
  override protected def doAction()(implicit gameState: GameState, turnPlayers: TurnPlayers, fastEffectTiming: FastEffectTiming, phase: Phase, step: Step) = {
    val toTribute = monster.owner.selectSummonMaterial(monster, monster.owner.field.monsterZones.toSeq.flatten)
    for (tribute <- toTribute) {
      tribute.owner.field.sendToGrave(tribute)
    }

    super.doAction()
  }
}
