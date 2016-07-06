package yugioh

import yugioh.action.monster.{Battle, DeclareAttack, TargetedForAttack}
import yugioh.card.monster.{Attack, Defense, Monster, Set}
import yugioh.events._

sealed trait Step

sealed trait BattlePhaseStep extends Step {
  self: EventsModuleComponent =>

  protected def emitStartEvent(): Unit = events.emit(BattlePhaseStepStartEvent(this))
  protected def emitEndEvent(): Unit = events.emit(BattlePhaseStepEndEvent(this))

  def next(gameState: GameState): BattlePhaseStep
}

object BattlePhaseStep {
  def loop(gameState: GameState) = {
    var battlePhaseStep: BattlePhaseStep = StartStep
    do {
      battlePhaseStep.emitStartEvent()
      val nextBattlePhaseStepAndMonster = battlePhaseStep.next(gameState)
      battlePhaseStep.emitEndEvent()
      battlePhaseStep = nextBattlePhaseStepAndMonster
    } while (battlePhaseStep != null)
  }
}

case object StartStep extends BattlePhaseStep with DefaultEventsModuleComponent {
  override def next(gameState: GameState): BattlePhaseStep = {
    FastEffectTiming.loop(gameState.copy(step = this))
    BattleStep
  }
}

case object BattleStep extends BattlePhaseStep with DefaultEventsModuleComponent {
  override def next(gameState: GameState): BattlePhaseStep = {
    var attacker: Monster = null
    var target: Monster = null
    val subscription = events.observe { event =>
      event match {
        case targeted: TargetedForAttack =>
          target = targeted.target
        case declareAttack: DeclareAttack =>
          attacker = declareAttack.attacker
        case ignore =>
      }
    }

    // listen for an attack declaration here
    FastEffectTiming.loop(gameState.copy(step = this))

    subscription.dispose()

    if (attacker != null) {
      BattleStepWithPendingAttack(Battle(attacker, target))
    } else {
      EndStep
    }
  }
}

case class BattleStepWithPendingAttack(battle: Battle) extends BattlePhaseStep with DefaultEventsModuleComponent {
  override def next(gameState: GameState): BattlePhaseStep = {
    FastEffectTiming.loop(gameState.copy(step = this), start = CheckForTrigger(Nil))
    // TODO: replays - BattleStepWithPendingAttack -> BattleStep instead of DamageStep
    DamageStep(battle)
  }
}

case class DamageStep(battle: Battle) extends BattlePhaseStep with DefaultEventsModuleComponent {
  override def next(gameState: GameState): BattlePhaseStep = {
    DamageStepSubStep.loop(battle)(gameState.copy(step = this))
    BattleStep
  }
}

case object EndStep extends BattlePhaseStep with DefaultEventsModuleComponent {
  override def next(gameState: GameState): BattlePhaseStep = {
    FastEffectTiming.loop(gameState.copy(step = this))
    null
  }
}


sealed trait DamageStepSubStep extends Step {
  def performAndGetNext(battle: Battle)(implicit gameState: GameState): DamageStepSubStep
}

object DamageStepSubStep extends DefaultEventsModuleComponent {
  def loop(battle: Battle)(implicit gameState: GameState): Unit = {
    var subStep: DamageStepSubStep = StartOfTheDamageStep
    do {
      events.emit(DamageSubStepStartEvent(subStep))
      val nextSubStep = subStep.performAndGetNext(battle)
      events.emit(DamageSubStepEndEvent(subStep))
      subStep = nextSubStep
    } while (subStep != null)
  }
}

// http://www.yugioh-card.com/uk/gameplay/damage.html
case object StartOfTheDamageStep extends DamageStepSubStep {
  override def performAndGetNext(battle: Battle)(implicit gameState: GameState) = {
    FastEffectTiming.loop(gameState.copy(step = this))
    BeforeDamageCalculation
  }
}

case object BeforeDamageCalculation extends DamageStepSubStep with DefaultEventsModuleComponent {
  override def performAndGetNext(battle: Battle)(implicit gameState: GameState) = {
    // flip the target if need be
    battle match {
      case Battle(_, target) if target != null =>
        for (controlledState <- target.maybeMonsterControlledState)
          if (controlledState.position == Set) {
            controlledState.position = Defense
            events.emit(FlippedRegular(target, battle))
          }
      case ignore =>
    }

    FastEffectTiming.loop(gameState.copy(step = this))
    PerformDamageCalculation
  }
}

case object PerformDamageCalculation extends DamageStepSubStep with DefaultEventsModuleComponent {
  override def performAndGetNext(battle: Battle)(implicit gameState: GameState) = {
    FastEffectTiming.loop(gameState.copy(step = this))

    var destroyed: Set[Monster] = collection.immutable.Set()
    val subscription = events.observe { event =>
      event match {
        case DestroyedByBattle(monster, _) =>
          destroyed += monster
        case ignore =>
      }
    }

    battle match {
      case Battle(attacker, null) =>
        events.emit(BattleDamage(gameState.turnPlayers.opponent, attacker.attack))
      case Battle(attacker, target) =>
        for (
          monsterControlledState <- target.maybeMonsterControlledState;
          position = monsterControlledState.position
        ) {
          position match {
            case Defense =>
              if (attacker.attack > target.defense) {
                // TODO LOW: piercing damage?
                events.emit(DestroyedByBattle(target, attacker))
              }
            case Attack =>
              // if both have 0 attack, nothing happens here
              if (attacker.attack != 0 || target.attack != 0) {
                val difference = attacker.attack - target.attack
                difference.signum match { // get the sign of the difference
                  case -1 => // attacker destroyed
                    events.emit(BattleDamage(gameState.turnPlayers.turnPlayer, -difference))
                    events.emit(DestroyedByBattle(attacker, target))
                  case 0 =>
                    events.emit(DestroyedByBattle(attacker, target))
                    events.emit(DestroyedByBattle(target, attacker))
                  case 1 => // target destroyed
                    events.emit(BattleDamage(gameState.turnPlayers.opponent, difference))
                    events.emit(DestroyedByBattle(target, attacker))
                }
              }
            case Set =>
              throw new IllegalStateException("Attacked monster should have been flipped face up.")
          }
        }
    }

    subscription.dispose()

    AfterDamageCalculation(destroyed)
  }
}

case class AfterDamageCalculation(destroyed: Set[Monster]) extends DamageStepSubStep {
  override def performAndGetNext(battle: Battle)(implicit gameState: GameState): DamageStepSubStep = {
    // TODO: after damage calculation
    FastEffectTiming.loop(gameState.copy(step = this))
    EndOfTheDamageStep(destroyed)
  }
}

case class EndOfTheDamageStep(destroyed: Set[Monster]) extends DamageStepSubStep {
  override def performAndGetNext(battle: Battle)(implicit gameState: GameState) = {
    for (monster <- destroyed) {
      monster.sendToGrave()
    }

    FastEffectTiming.loop(gameState.copy(step = this))
    null
  }
}
