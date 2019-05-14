package yugioh.action.monster

import yugioh._
import yugioh.action._
import yugioh.card.monster._
import yugioh.card.state._
import yugioh.events.{EventsModule, TimeSeparationEvent}

sealed trait SummonOrSet extends InherentAction {
  val monster: Monster
  val player: Player = monster.Owner

  override def toString = s"${this.getClass.getSimpleName}($monster)"

  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    // super should be called for this line after the monster is placed on the field!
    monster.maybeControlledState.get.manuallyChangedPositionsThisTurn = true
  }
}

trait Summon extends SummonOrSet

sealed trait NormalOrFlipSummon extends Summon

trait NormalSummon extends NormalOrFlipSummon

class NormalSummonImpl(override val cause: Cause, override val monster: Monster) extends NormalSummon {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    monster.Owner.field.placeAsMonster(cause, monster, Attack, NormalSummoned)
    super.doAction()
  }
}

trait TributeSummon extends NormalSummon

class TributeSummonImpl(cause: Cause, override val monster: Monster) extends NormalSummonImpl(cause, monster) with TributeSummon {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    val toTribute = monster.Owner.selectSummonMaterial(TributeSummonCriteria(monster.Owner, monster))
    for (tribute <- toTribute) {
      // this might not be right, I couldn't think of a time where it matters to figure it out
      tribute.sendToGrave(GameMechanics)
      // TODO: (create and) emit an event for UsedForTributeSummon or something
    }

    eventsModule.emit(TimeSeparationEvent)

    // TODO LOW: cleanup this kludge where useless operations are done
    super.doAction()
    monster.maybeMonsterFieldState.get.howSummoned = TributeSummoned
  }
}

trait FlipSummon extends NormalOrFlipSummon with SwitchPosition

class FlipSummonImpl(override val cause: Cause, override val monster: Monster)(implicit override val eventsModule: EventsModule) extends FlipSummon {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    super.doAction()
  }
}

trait SetAsMonster extends SummonOrSet with SetCard

class SetAsMonsterImpl(override val cause: Cause, override val monster: Monster) extends SetAsMonster {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    monster.Owner.field.placeAsMonster(cause, monster, Set, NotSummoned)
    super.doAction()
  }
}

trait TributeSet extends SetAsMonster

class TributeSetImpl(cause: Cause, monster: Monster) extends SetAsMonsterImpl(cause, monster) with TributeSet {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    val summonCriteria = TributeSummonCriteria(monster.Owner, monster)
    val toTribute = monster.Owner.selectSummonMaterial(summonCriteria)
    for (tribute <- toTribute) {
      tribute.sendToGrave(cause)
    }

    super.doAction()
  }
}

trait SpecialSummon extends Summon {
  val position: Position
}

case class SpecialSummonImpl(cause: Cause, override val player: Player, monster: Monster, position: Position) extends SpecialSummon {
  override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    player.field.placeAsMonster(cause, monster, position, SpecialSummoned)
    super.doAction()
  }
}
