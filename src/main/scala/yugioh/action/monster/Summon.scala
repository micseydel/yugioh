package yugioh.action.monster

import yugioh._
import yugioh.action.{ActionModule, InherentAction, SetCard}
import yugioh.card.monster._
import yugioh.card.state._
import yugioh.events.{EventsModule, TimeSeparationEvent}

sealed trait SummonOrSet extends InherentAction {
  val monster: Monster
  val player = monster.Owner

  override def toString = s"${this.getClass.getSimpleName}($monster)"

  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    // super should be called for this line after the monster is placed on the field!
    monster.maybeControlledState.get.manuallyChangedPositionsThisTurn = true
  }
}

trait Summon extends SummonOrSet

sealed trait NormalOrFlipSummon extends Summon

trait NormalSummon extends NormalOrFlipSummon

class NormalSummonImpl(val monster: Monster) extends NormalSummon {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    monster.Owner.field.placeAsMonster(monster, Attack, NormalSummoned)
    super.doAction()
  }
}

trait TributeSummon extends NormalSummon

class TributeSummonImpl(override val monster: Monster) extends NormalSummonImpl(monster) with TributeSummon {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    val toTribute = monster.Owner.selectSummonMaterial(TributeSummonCriteria(monster.Owner, monster))
    for (tribute <- toTribute) {
      tribute.sendToGrave()
      // TODO: (create and) emit an event for UsedForTributeSummon or something
    }

    eventsModule.emit(TimeSeparationEvent)

    // TODO LOW: cleanup this kludge where useless operations are done
    super.doAction()
    monster.maybeMonsterFieldState.get.howSummoned = TributeSummoned
  }
}

trait FlipSummon extends NormalOrFlipSummon with SwitchPosition

class FlipSummonImpl(override val monster: Monster)(implicit override val eventsModule: EventsModule) extends FlipSummon {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    super.doAction()
  }
}

trait SetAsMonster extends SummonOrSet with SetCard

class SetAsMonsterImpl(override val monster: Monster) extends SetAsMonster {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    monster.Owner.field.placeAsMonster(monster, Set, NotSummoned)
    super.doAction()
  }
}

trait TributeSet extends SetAsMonster

class TributeSetImpl(monster: Monster) extends SetAsMonsterImpl(monster) with TributeSet {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    val summonCriteria = TributeSummonCriteria(monster.Owner, monster)
    val toTribute = monster.Owner.selectSummonMaterial(summonCriteria)
    for (tribute <- toTribute) {
      tribute.sendToGrave()
    }

    super.doAction()
  }
}

trait SpecialSummon extends Summon {
  val position: Position
}

case class SpecialSummonImpl(override val player: Player, monster: Monster, position: Position) extends SpecialSummon {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    player.field.placeAsMonster(monster, position, SpecialSummoned)
    super.doAction()
  }
}
