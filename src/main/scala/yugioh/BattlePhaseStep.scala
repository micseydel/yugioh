package yugioh

import yugioh.action.monster.{Battle, DeclareAttack, TargetedForAttack}
import yugioh.card.monster.{Attack, Defense, Monster, Set}
import yugioh.events._

sealed trait Step

trait BattlePhaseModule {
  def loop(gameState: GameState)(implicit eventsModule: EventsModule)
}

trait BattlePhaseModuleComponent {
  implicit def battlePhaseModule: BattlePhaseModule
}

trait DefaultBattlePhaseModuleComponent extends BattlePhaseModuleComponent {
  override def battlePhaseModule: BattlePhaseModule = new BattlePhaseModule {
    def loop(gameState: GameState)(implicit eventsModule: EventsModule) = {
      var battlePhaseStep: BattlePhaseStep = StartStep
      do {
        battlePhaseStep.emitStartEvent()
        val nextBattlePhaseStepAndMonster = battlePhaseStep.next(gameState)
        battlePhaseStep.emitEndEvent()
        battlePhaseStep = nextBattlePhaseStepAndMonster
      } while (battlePhaseStep != null)
    }
  }
}


sealed trait BattlePhaseStep extends Step {
  def emitStartEvent()(implicit eventsModule: EventsModule): Unit = eventsModule.emit(BattlePhaseStepStartEvent(this))
  def emitEndEvent()(implicit eventsModule: EventsModule): Unit = eventsModule.emit(BattlePhaseStepEndEvent(this))

  def next(gameState: GameState)(implicit eventsModule: EventsModule): BattlePhaseStep
}

case object StartStep extends BattlePhaseStep {
  override def next(gameState: GameState)(implicit eventsModule: EventsModule): BattlePhaseStep = {
    FastEffectTiming.loop(gameState.copy(step = this))
    BattleStep
  }
}

case object BattleStep extends BattlePhaseStep {
  override def next(gameState: GameState)(implicit eventsModule: EventsModule): BattlePhaseStep = {
    var attacker: Monster = null
    var target: Monster = null
    val subscription = eventsModule.observe { event =>
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

case class BattleStepWithPendingAttack(battle: Battle) extends BattlePhaseStep {
  override def next(gameState: GameState)(implicit eventsModule: EventsModule): BattlePhaseStep = {
    FastEffectTiming.loop(gameState.copy(step = this), start = CheckForTrigger(Nil))
    // TODO: replays - BattleStepWithPendingAttack -> BattleStep instead of DamageStep
    DamageStep(battle)
  }
}

case class DamageStep(battle: Battle) extends BattlePhaseStep {
  override def next(gameState: GameState)(implicit eventsModule: EventsModule): BattlePhaseStep = {
    DamageStepSubStep.loop(battle)(gameState.copy(step = this), eventsModule)
    BattleStep
  }
}

case object EndStep extends BattlePhaseStep {
  override def next(gameState: GameState)(implicit eventsModule: EventsModule): BattlePhaseStep = {
    FastEffectTiming.loop(gameState.copy(step = this))
    null
  }
}


sealed trait DamageStepSubStep extends Step {
  def performAndGetNext(battle: Battle)(implicit gameState: GameState, eventsModule: EventsModule): DamageStepSubStep
}

object DamageStepSubStep {
  def loop(battle: Battle)(implicit gameState: GameState, eventsModule: EventsModule): Unit = {
    var subStep: DamageStepSubStep = StartOfTheDamageStep
    do {
      eventsModule.emit(DamageSubStepStartEvent(subStep))
      val nextSubStep = subStep.performAndGetNext(battle)
      eventsModule.emit(DamageSubStepEndEvent(subStep))
      subStep = nextSubStep
    } while (subStep != null)
  }
}

// http://www.yugioh-card.com/uk/gameplay/damage.html
case object StartOfTheDamageStep extends DamageStepSubStep {
  override def performAndGetNext(battle: Battle)(implicit gameState: GameState, eventsModule: EventsModule) = {
    FastEffectTiming.loop(gameState.copy(step = this))
    BeforeDamageCalculation
  }
}

case object BeforeDamageCalculation extends DamageStepSubStep {
  override def performAndGetNext(battle: Battle)(implicit gameState: GameState, eventsModule: EventsModule) = {
    // flip the target if need be
    battle match {
      case Battle(_, target) if target != null =>
        for (controlledState <- target.maybeMonsterControlledState)
          if (controlledState.position == Set) {
            controlledState.position = Defense
            eventsModule.emit(FlippedRegular(target, battle))
          }
      case ignore =>
    }

    FastEffectTiming.loop(gameState.copy(step = this))
    PerformDamageCalculation
  }
}

case object PerformDamageCalculation extends DamageStepSubStep {
  override def performAndGetNext(battle: Battle)(implicit gameState: GameState, eventsModule: EventsModule) = {
    FastEffectTiming.loop(gameState.copy(step = this))

    var destroyed: Set[Monster] = collection.immutable.Set()
    val subscription = eventsModule.observe { event =>
      event match {
        case DestroyedByBattle(monster, _) =>
          destroyed += monster
        case ignore =>
      }
    }

    battle match {
      case Battle(attacker, null) =>
        eventsModule.emit(BattleDamage(gameState.turnPlayers.opponent, attacker.attack))
      case Battle(attacker, target) =>
        for (
          monsterControlledState <- target.maybeMonsterControlledState;
          position = monsterControlledState.position
        ) {
          position match {
            case Defense =>
              if (attacker.attack > target.defense) {
                // TODO LOW: piercing damage?
                eventsModule.emit(DestroyedByBattle(target, attacker))
              }
            case Attack =>
              // if both have 0 attack, nothing happens here
              if (attacker.attack != 0 || target.attack != 0) {
                val difference = attacker.attack - target.attack
                difference.signum match { // get the sign of the difference
                  case -1 => // attacker destroyed
                    eventsModule.emit(BattleDamage(gameState.turnPlayers.turnPlayer, -difference))
                    eventsModule.emit(DestroyedByBattle(attacker, target))
                  case 0 =>
                    eventsModule.emit(DestroyedByBattle(attacker, target))
                    eventsModule.emit(DestroyedByBattle(target, attacker))
                  case 1 => // target destroyed
                    eventsModule.emit(BattleDamage(gameState.turnPlayers.opponent, difference))
                    eventsModule.emit(DestroyedByBattle(target, attacker))
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
  override def performAndGetNext(battle: Battle)(implicit gameState: GameState, eventsModule: EventsModule): DamageStepSubStep = {
    // TODO: after damage calculation
    FastEffectTiming.loop(gameState.copy(step = this))
    EndOfTheDamageStep(destroyed)
  }
}

case class EndOfTheDamageStep(destroyed: Set[Monster]) extends DamageStepSubStep {
  override def performAndGetNext(battle: Battle)(implicit gameState: GameState, eventsModule: EventsModule) = {
    for (monster <- destroyed) {
      monster.sendToGrave()
    }

    FastEffectTiming.loop(gameState.copy(step = this))
    null
  }
}
